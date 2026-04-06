"""Gemma 4 E4B audio transcription server.

Loads the model at startup and serves transcription requests via HTTP.
Validation setup until Ollama supports gemma4 audio input — same model
weights, so transcription quality is equivalent.

Usage:
  POST / with raw audio body, optional X-Language header, returns {"text": "..."}
  GET /health returns 200
"""
import http.server
import json
import os
import subprocess
import sys
import tempfile

os.environ.setdefault("OMP_NUM_THREADS", "4")
os.environ.setdefault("MKL_NUM_THREADS", "4")

LISTEN_HOST = os.environ.get("LISTEN_HOST", "127.0.0.1")
LISTEN_PORT = int(os.environ.get("LISTEN_PORT", "8180"))
MODEL_ID = os.environ.get("GEMMA4_MODEL", "google/gemma-4-E4B-it")

print(f"Loading {MODEL_ID}...", file=sys.stderr)

import torch
from transformers import AutoProcessor, AutoModelForImageTextToText, BitsAndBytesConfig

torch.set_num_threads(4)
torch.set_num_interop_threads(1)

quantization_config = BitsAndBytesConfig(
    load_in_4bit=True,
    bnb_4bit_quant_type="nf4",
    bnb_4bit_compute_dtype=torch.float16,
)

processor = AutoProcessor.from_pretrained(MODEL_ID)
model = AutoModelForImageTextToText.from_pretrained(
    MODEL_ID,
    device_map="cpu",
    quantization_config=quantization_config,
    low_cpu_mem_usage=True,
)

print(f"Model loaded ({MODEL_ID})", file=sys.stderr)


class Handler(http.server.BaseHTTPRequestHandler):
    def do_POST(self):
        content_length = int(self.headers.get("Content-Length", 0))
        if content_length == 0:
            self._respond(400, {"error": "No audio data"})
            return

        language = self.headers.get("X-Language", "").strip() or None
        body = self.rfile.read(content_length)

        with tempfile.NamedTemporaryFile(suffix=".audio", delete=False) as f:
            f.write(body)
            raw_path = f.name

        wav_fd, wav_path = tempfile.mkstemp(suffix=".wav")
        os.close(wav_fd)
        try:
            subprocess.run(
                ["ffmpeg", "-y", "-i", raw_path, "-t", "30", "-ar", "16000", "-ac", "1", "-f", "wav", wav_path],
                capture_output=True, check=True, timeout=30,
            )

            lang_hint = f" in {language}" if language and language != "auto" else ""
            prompt = (
                f"Transcribe the following speech segment{lang_hint} into text.\n\n"
                "Follow these specific instructions for formatting the answer:\n"
                "* Only output the transcription, with no newlines.\n"
                "* When transcribing numbers, write the digits, i.e. write 1.7 and not one point seven, and write 3 instead of three."
            )

            messages = [{
                "role": "user",
                "content": [
                    {"type": "audio", "audio": wav_path},
                    {"type": "text", "text": prompt},
                ],
            }]

            inputs = processor.apply_chat_template(
                messages,
                add_generation_prompt=True,
                tokenize=True,
                return_dict=True,
                return_tensors="pt",
                enable_thinking=False,
            )

            for k, v in inputs.items():
                if torch.is_tensor(v):
                    if v.is_floating_point():
                        inputs[k] = v.to(model.device, dtype=model.dtype)
                    else:
                        inputs[k] = v.to(model.device)

            with torch.inference_mode():
                outputs = model.generate(**inputs, max_new_tokens=256, do_sample=False)

            input_len = inputs["input_ids"].shape[-1]
            text = processor.decode(outputs[0][input_len:], skip_special_tokens=True)

            self._respond(200, {"text": text.strip()})
        except subprocess.CalledProcessError:
            self._respond(502, {"error": "Audio conversion failed"})
        except Exception as e:
            print(f"Gemma4 error: {e}", file=sys.stderr)
            self._respond(502, {"error": f"Transcription failed: {e}"})
        finally:
            for p in [raw_path, wav_path]:
                if os.path.exists(p):
                    os.unlink(p)

    def do_GET(self):
        if self.path == "/health":
            self._respond(200, {"status": "ok"})
            return
        self._respond(404, {"error": "not found"})

    def _respond(self, code, data):
        body = json.dumps(data).encode()
        try:
            self.send_response(code)
            self.send_header("Content-Type", "application/json")
            self.send_header("Content-Length", str(len(body)))
            self.end_headers()
            self.wfile.write(body)
        except (BrokenPipeError, ConnectionResetError):
            pass

    def log_message(self, format, *args):
        print(f"{self.client_address[0]} - {format % args}", file=sys.stderr)


if __name__ == "__main__":
    server = http.server.HTTPServer((LISTEN_HOST, LISTEN_PORT), Handler)
    print(f"Gemma 4 server listening on {LISTEN_HOST}:{LISTEN_PORT}", file=sys.stderr)
    server.serve_forever()
