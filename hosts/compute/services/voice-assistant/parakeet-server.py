"""Parakeet TDT 0.6B v3 transcription server (ONNX Runtime).

Loads the INT8 ONNX model at startup via onnx-asr and serves an
OpenAI-compatible /v1/audio/transcriptions endpoint.

Usage:
  POST /v1/audio/transcriptions with multipart file → {"text": "..."}
  GET /health → 200
"""
import email.parser
import http.server
import json
import os
import subprocess
import sys
import tempfile

LISTEN_HOST = os.environ.get("LISTEN_HOST", "127.0.0.1")
LISTEN_PORT = int(os.environ.get("LISTEN_PORT", "5092"))
MODEL_NAME = os.environ.get("PARAKEET_MODEL", "nemo-parakeet-tdt-0.6b-v3")
MAX_UPLOAD_BYTES = int(os.environ.get("MAX_UPLOAD_BYTES", str(50 * 1024 * 1024)))

print(f"Loading Parakeet TDT model ({MODEL_NAME})...", file=sys.stderr)

import onnx_asr
import onnxruntime as ort

sess_options = ort.SessionOptions()
sess_options.intra_op_num_threads = 4
sess_options.inter_op_num_threads = 1
sess_options.execution_mode = ort.ExecutionMode.ORT_SEQUENTIAL
sess_options.graph_optimization_level = ort.GraphOptimizationLevel.ORT_ENABLE_ALL

asr_model = onnx_asr.load_model(
    MODEL_NAME,
    quantization="int8",
    providers=["CPUExecutionProvider"],
    sess_options=sess_options,
)

print(f"Model loaded ({MODEL_NAME})", file=sys.stderr)


class Handler(http.server.BaseHTTPRequestHandler):
    def do_POST(self):
        if self.path != "/v1/audio/transcriptions":
            self._respond(404, {"error": "not found"})
            return

        content_length = int(self.headers.get("Content-Length", 0))
        if content_length == 0:
            self._respond(400, {"error": "No data"})
            return
        if content_length > MAX_UPLOAD_BYTES:
            self._respond(413, {"error": "Upload too large"})
            return

        content_type = self.headers.get("Content-Type", "")
        body = self.rfile.read(content_length)

        file_data = None
        response_format = "json"
        if "multipart/form-data" in content_type:
            parts = self._parse_multipart(content_type, body)
            file_data = parts.get("file")
            response_format = parts.get("response_format", "json")
        else:
            file_data = body

        if not file_data:
            self._respond(400, {"error": "Missing 'file' field"})
            return

        print(f"Transcribe: {len(file_data)} bytes", file=sys.stderr)

        with tempfile.NamedTemporaryFile(suffix=".audio", delete=False) as f:
            f.write(file_data)
            raw_path = f.name

        wav_path = raw_path + ".wav"
        try:
            subprocess.run(
                ["ffmpeg", "-y", "-i", raw_path, "-ar", "16000", "-ac", "1", "-f", "wav", wav_path],
                capture_output=True, check=True, timeout=30,
            )

            text = asr_model.recognize(wav_path)
            text = text.strip() if text else ""

            if response_format == "text":
                try:
                    self.send_response(200)
                    self.send_header("Content-Type", "text/plain")
                    encoded = text.encode()
                    self.send_header("Content-Length", str(len(encoded)))
                    self.end_headers()
                    self.wfile.write(encoded)
                except (BrokenPipeError, ConnectionResetError):
                    pass
            else:
                self._respond(200, {"text": text})

        except subprocess.CalledProcessError:
            self._respond(502, {"error": "Audio conversion failed"})
        except Exception as e:
            print(f"Parakeet error: {e}", file=sys.stderr)
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

    @staticmethod
    def _parse_multipart(content_type, body):
        header = f"MIME-Version: 1.0\r\nContent-Type: {content_type}\r\n\r\n"
        msg = email.parser.BytesParser().parsebytes(header.encode() + body)
        fields = {}
        for part in msg.walk():
            name = part.get_param("name", header="content-disposition")
            if name is None:
                continue
            payload = part.get_payload(decode=True)
            if payload is None:
                continue
            if name == "file":
                fields["file"] = payload
            else:
                fields[name] = payload.decode().strip()
        if "file" not in fields:
            for part in msg.walk():
                payload = part.get_payload(decode=True)
                if payload and len(payload) > 100:
                    fields["file"] = payload
                    break
        return fields

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
    server = http.server.ThreadingHTTPServer((LISTEN_HOST, LISTEN_PORT), Handler)
    print(f"Parakeet server listening on {LISTEN_HOST}:{LISTEN_PORT}", file=sys.stderr)
    server.serve_forever()
