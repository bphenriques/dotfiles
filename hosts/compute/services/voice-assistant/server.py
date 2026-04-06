"""Voice assistant: receives audio, transcribes via whisper-server, sends to ntfy.

Accepts multipart/form-data with a 'file' field (same as whisper-server).
Forwards to whisper-server, sends the transcript to ntfy, and returns JSON.

Usage:
  curl -F "file=@recording.wav" http://host:port/
"""
import email.parser
import http.server
import json
import os
import subprocess
import sys
import tempfile

LISTEN_HOST = os.environ.get("LISTEN_HOST", "127.0.0.1")
LISTEN_PORT = int(os.environ.get("LISTEN_PORT", "8179"))
WHISPER_URL = os.environ["WHISPER_URL"]
WHISPER_EN_URL = os.environ.get("WHISPER_EN_URL", "")
NTFY_URL = os.environ["NTFY_URL"]
NTFY_TOKEN_FILE = os.environ["NTFY_TOKEN_FILE"]
CURL = os.environ.get("CURL_PATH", "curl")
WYOMING_HOST = os.environ.get("WYOMING_HOST", "127.0.0.1")
WYOMING_PORT = os.environ.get("WYOMING_PORT", "10300")
WYOMING_SCRIPT = os.environ.get("WYOMING_SCRIPT", "")
PYTHON = os.environ.get("PYTHON_PATH", sys.executable)
GEMMA4_URL = os.environ.get("GEMMA4_URL", "")
OLLAMA_URL = os.environ.get("OLLAMA_URL", "")
OLLAMA_MODEL = os.environ.get("OLLAMA_MODEL", "gemma3:4b")
PARAKEET_URL = os.environ.get("PARAKEET_URL", "")
MAX_UPLOAD_BYTES = int(os.environ.get("MAX_UPLOAD_BYTES", str(20 * 1024 * 1024)))

UPLOAD_PAGE = """<!DOCTYPE html>
<html><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">
<title>Voice Assistant</title>
<style>
  body { font-family: system-ui; max-width: 480px; margin: 2rem auto; padding: 0 1rem; }
  form { display: flex; flex-direction: column; gap: 1rem; }
  input[type=file], select { font-size: 1rem; padding: 0.4rem; }
  button { font-size: 1.2rem; padding: 0.8rem; cursor: pointer; }
  #result { margin-top: 1rem; padding: 1rem; background: #f0f0f0; border-radius: 8px; display: none; white-space: pre-wrap; }
  #status { margin-top: 0.5rem; color: #666; }
  label { font-weight: 600; }
</style></head>
<body>
<h2>Voice Assistant</h2>
<form id="f">
  <input type="file" name="file" accept="audio/*,image/*" capture="environment" required>
  <input type="text" name="prompt" placeholder="Prompt (optional, for images)" style="font-size:1rem;padding:0.4rem">
  <label>Language
    <select name="language">
      <option value="en" selected>English</option>
      <option value="pt">Português</option>
      <option value="auto">Auto-detect</option>
    </select>
  </label>
  <div style="display:flex;gap:0.5rem;flex-wrap:wrap">
    <button type="submit" name="engine" value="whisper-cpp" style="flex:1">whisper.cpp</button>
    <button type="submit" name="engine" value="whisper-en" style="flex:1">whisper.en</button>
    <button type="submit" name="engine" value="faster-whisper" style="flex:1">faster-whisper</button>
    <button type="submit" name="engine" value="parakeet" style="flex:1">parakeet</button>
    <button type="submit" name="engine" value="enhance" style="flex:1 1 100%">whisper.en + LLM</button>
    <button type="submit" name="engine" value="parakeet-llm" style="flex:1 1 100%">parakeet + LLM</button>
  </div>
</form>
<div id="status"></div>
<div id="result"></div>
<script>
document.querySelectorAll('button[name=engine]').forEach(btn => {
  btn.onclick = (e) => { document.getElementById('f').dataset.engine = btn.value; };
});
document.getElementById('f').onsubmit = async (e) => {
  e.preventDefault();
  const status = document.getElementById('status');
  const result = document.getElementById('result');
  const engine = e.target.dataset.engine || 'whisper-cpp';
  const url = engine === 'faster-whisper' ? '/benchmark' : engine === 'gemma4' ? '/gemma4' : engine === 'enhance' ? '/enhance' : engine === 'parakeet' ? '/parakeet' : engine === 'whisper-en' ? '/whisper-en' : engine === 'parakeet-llm' ? '/parakeet-llm' : '/';
  status.textContent = (engine === 'enhance' || engine === 'parakeet-llm' ? 'Processing' : 'Transcribing') + ' (' + engine + ')...';
  result.style.display = 'none';
  const t0 = Date.now();
  try {
    const fd = new FormData(e.target);
    const r = await fetch(url, { method: 'POST', body: fd });
    const j = await r.json();
    const elapsed = ((Date.now() - t0) / 1000).toFixed(1);
    if (j.text) { result.textContent = j.text; result.style.display = 'block'; status.textContent = engine + ': ' + elapsed + 's'; }
    else if (j.error) { status.textContent = 'Error: ' + j.error; }
    else { status.textContent = 'Empty transcript'; }
  } catch (err) { status.textContent = 'Error: ' + err.message; }
};
</script>
</body></html>"""


def read_token():
    with open(NTFY_TOKEN_FILE) as f:
        return f.read().strip()


class Handler(http.server.BaseHTTPRequestHandler):
    def _read_upload(self):
        """Read and parse the upload, returning (file_data, language, prompt) or None on error."""
        content_length = int(self.headers.get("Content-Length", 0))
        if content_length == 0:
            self._error(400, "No data")
            return None, None, None
        if content_length > MAX_UPLOAD_BYTES:
            self._error(413, "Upload too large")
            return None, None, None

        content_type = self.headers.get("Content-Type", "")
        body = self.rfile.read(content_length)

        language = None
        prompt = None
        if "multipart/form-data" in content_type:
            parts = self._parse_multipart(content_type, body)
            file_data = parts.get("file")
            language = parts.get("language")
            prompt = parts.get("prompt") or None
            if file_data is None:
                self._error(400, "Missing 'file' field in multipart data")
                return None, None, None
            print(f"Upload: {len(file_data)} bytes, lang={language}", file=sys.stderr)
        else:
            file_data = body
            print(f"Upload: {len(file_data)} bytes raw", file=sys.stderr)

        return file_data, language, prompt

    def do_POST(self):
        if self.path == "/benchmark":
            return self._handle_benchmark()
        if self.path == "/gemma4":
            return self._handle_gemma4()
        if self.path == "/enhance":
            return self._handle_enhance()
        if self.path == "/parakeet":
            return self._handle_parakeet()
        if self.path == "/whisper-en":
            return self._handle_whisper_en()
        if self.path == "/parakeet-llm":
            return self._handle_parakeet_llm()
        return self._handle_whisper_cpp()

    def _handle_benchmark(self):
        """Transcribe via faster-whisper (Wyoming protocol)."""
        audio_data, language, _prompt = self._read_upload()
        if audio_data is None:
            return
        if self._looks_like_image(audio_data):
            self._error(400, "Images are only supported on the whisper + LLM endpoint")
            return

        with tempfile.NamedTemporaryFile(suffix=".audio", delete=False) as f:
            f.write(audio_data)
            tmp_path = f.name

        try:
            cmd = [PYTHON, WYOMING_SCRIPT, WYOMING_HOST, WYOMING_PORT, tmp_path]
            if language and language != "auto":
                cmd.append(language)
            result = subprocess.run(cmd, capture_output=True, text=True, timeout=120)
        finally:
            os.unlink(tmp_path)

        if result.returncode != 0:
            print(f"faster-whisper error: {result.stderr[:200]}", file=sys.stderr)
            self._error(502, "faster-whisper transcription failed")
            return

        try:
            data = json.loads(result.stdout)
        except json.JSONDecodeError:
            self._error(502, "Invalid response from faster-whisper")
            return

        transcript = data.get("text", "").strip()
        if not transcript:
            self._error(200, "Empty transcript")
            return

        response_body = json.dumps({"text": transcript}).encode()
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(response_body)))
        self.end_headers()
        self.wfile.write(response_body)

    def _handle_gemma4(self):
        """Transcribe via Gemma 4 E4B (native audio)."""
        if not GEMMA4_URL:
            self._error(503, "Gemma 4 not configured")
            return

        audio_data, language, _prompt = self._read_upload()
        if audio_data is None:
            return
        if self._looks_like_image(audio_data):
            self._error(400, "Images are only supported on the whisper + LLM endpoint")
            return

        with tempfile.NamedTemporaryFile(suffix=".audio", delete=False) as f:
            f.write(audio_data)
            tmp_path = f.name

        try:
            curl_cmd = [CURL, "-sf", "--max-time", "180"]
            if language and language != "auto":
                curl_cmd += ["-H", f"X-Language: {language}"]
            curl_cmd += ["--data-binary", f"@{tmp_path}", GEMMA4_URL]
            result = subprocess.run(curl_cmd, capture_output=True, text=True, timeout=200)
        finally:
            os.unlink(tmp_path)

        if result.returncode != 0:
            print(f"gemma4 error: {result.stderr[:200]}", file=sys.stderr)
            self._error(502, "Gemma 4 transcription failed")
            return

        try:
            data = json.loads(result.stdout)
        except json.JSONDecodeError:
            self._error(502, "Invalid response from Gemma 4")
            return

        transcript = data.get("text", "").strip()
        if not transcript:
            self._error(200, "Empty transcript")
            return

        response_body = json.dumps({"text": transcript}).encode()
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(response_body)))
        self.end_headers()
        self.wfile.write(response_body)

    def _handle_enhance(self):
        """Transcribe via whisper.en + enhance with Ollama, or describe image."""
        if not OLLAMA_URL:
            self._error(503, "Ollama not configured")
            return
        if not WHISPER_EN_URL:
            self._error(503, "Whisper English-only server not configured")
            return

        file_data, language, user_prompt = self._read_upload()
        if file_data is None:
            return

        is_image = self._looks_like_image(file_data)

        if is_image:
            import base64, urllib.request
            image_b64 = base64.b64encode(file_data).decode()
            if user_prompt:
                prompt = user_prompt
            else:
                prompt = "Describe this image in detail."
            if language and language != "auto":
                prompt += f" Respond in {language}."
            payload = json.dumps({
                "model": OLLAMA_MODEL,
                "messages": [{"role": "user", "content": prompt, "images": [image_b64]}],
                "stream": False,
            }).encode()
            try:
                req = urllib.request.Request(
                    f"{OLLAMA_URL}/api/chat", data=payload,
                    headers={"Content-Type": "application/json"},
                )
                with urllib.request.urlopen(req, timeout=300) as resp:
                    data = json.loads(resp.read())
                text = data.get("message", {}).get("content", "").strip()
            except Exception as e:
                print(f"Ollama error: {e}", file=sys.stderr)
                self._error(502, f"Ollama request failed: {e}")
                return
        else:
            # Audio: transcribe via whisper.en first
            with tempfile.NamedTemporaryFile(suffix=".audio", delete=False) as f:
                f.write(file_data)
                tmp_path = f.name
            try:
                curl_cmd = [
                    CURL, "-sf",
                    "-F", f"file=@{tmp_path}",
                    "-F", "response_format=json",
                    "-F", "language=en",
                ]
                curl_cmd.append(WHISPER_EN_URL)
                result = subprocess.run(curl_cmd, capture_output=True, text=True, timeout=60)
            finally:
                os.unlink(tmp_path)

            if result.returncode != 0:
                print(f"whisper-server error: {result.stderr[:200]}", file=sys.stderr)
                self._error(502, "Transcription failed")
                return
            try:
                whisper_data = json.loads(result.stdout)
            except json.JSONDecodeError:
                self._error(502, "Invalid response from whisper-server")
                return
            transcript = whisper_data.get("text", "").strip()
            if not transcript:
                self._error(200, "Empty transcript")
                return

            # Enhance via Ollama
            import urllib.request
            lang_hint = f" in {language}" if language and language != "auto" else ""
            prompt = f"Clean up this voice transcript{lang_hint}. Fix punctuation, grammar, and remove filler words. Keep the original meaning. Only output the cleaned text.\n\n{transcript}"
            payload = json.dumps({
                "model": OLLAMA_MODEL,
                "messages": [{"role": "user", "content": prompt}],
                "stream": False,
            }).encode()
            try:
                req = urllib.request.Request(
                    f"{OLLAMA_URL}/api/chat", data=payload,
                    headers={"Content-Type": "application/json"},
                )
                with urllib.request.urlopen(req, timeout=300) as resp:
                    data = json.loads(resp.read())
                text = data.get("message", {}).get("content", "").strip()
            except Exception as e:
                print(f"Ollama error: {e}", file=sys.stderr)
                self._error(502, "Ollama enhancement failed")
                return

        if not text:
            self._error(200, "Empty result")
            return

        # Send to ntfy (best-effort)
        title = "Image Description" if is_image else "Voice Note"
        try:
            token = read_token()
            subprocess.run(
                [CURL, "-sf", "--max-time", "10",
                 "-H", f"Authorization: Bearer {token}",
                 "-H", f"Title: {title}",
                 "-H", "Tags: studio_microphone",
                 "-d", text,
                 NTFY_URL],
                capture_output=True, timeout=15,
            )
        except (subprocess.TimeoutExpired, OSError):
            pass

        response_body = json.dumps({"text": text}).encode()
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(response_body)))
        self.end_headers()
        self.wfile.write(response_body)

    def _handle_parakeet(self):
        """Transcribe via Parakeet TDT 0.6B v3 (ONNX Runtime)."""
        if not PARAKEET_URL:
            self._error(503, "Parakeet not configured")
            return

        audio_data, language, _prompt = self._read_upload()
        if audio_data is None:
            return
        if self._looks_like_image(audio_data):
            self._error(400, "Images are only supported on the whisper + LLM endpoint")
            return

        with tempfile.NamedTemporaryFile(suffix=".audio", delete=False) as f:
            f.write(audio_data)
            tmp_path = f.name

        try:
            curl_cmd = [
                CURL, "-sf", "--max-time", "120",
                "-F", f"file=@{tmp_path}",
                "-F", "response_format=json",
            ]
            curl_cmd.append(PARAKEET_URL)
            result = subprocess.run(curl_cmd, capture_output=True, text=True, timeout=130)
        finally:
            os.unlink(tmp_path)

        if result.returncode != 0:
            print(f"parakeet error: {result.stderr[:200]}", file=sys.stderr)
            self._error(502, "Parakeet transcription failed")
            return

        try:
            data = json.loads(result.stdout)
        except json.JSONDecodeError:
            self._error(502, "Invalid response from Parakeet")
            return

        transcript = data.get("text", "").strip()
        if not transcript:
            self._error(200, "Empty transcript")
            return

        response_body = json.dumps({"text": transcript}).encode()
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(response_body)))
        self.end_headers()
        self.wfile.write(response_body)

    def _handle_parakeet_llm(self):
        """Transcribe via Parakeet, then enhance with Ollama."""
        if not PARAKEET_URL:
            self._error(503, "Parakeet not configured")
            return
        if not OLLAMA_URL:
            self._error(503, "Ollama not configured")
            return

        file_data, language, _prompt = self._read_upload()
        if file_data is None:
            return
        if self._looks_like_image(file_data):
            self._error(400, "Images are only supported on the whisper.en + LLM endpoint")
            return

        # Step 1: transcribe via Parakeet
        with tempfile.NamedTemporaryFile(suffix=".audio", delete=False) as f:
            f.write(file_data)
            tmp_path = f.name

        try:
            curl_cmd = [
                CURL, "-sf", "--max-time", "120",
                "-F", f"file=@{tmp_path}",
                "-F", "response_format=json",
            ]
            curl_cmd.append(PARAKEET_URL)
            result = subprocess.run(curl_cmd, capture_output=True, text=True, timeout=130)
        finally:
            os.unlink(tmp_path)

        if result.returncode != 0:
            print(f"parakeet error: {result.stderr[:200]}", file=sys.stderr)
            self._error(502, "Parakeet transcription failed")
            return

        try:
            parakeet_data = json.loads(result.stdout)
        except json.JSONDecodeError:
            self._error(502, "Invalid response from Parakeet")
            return

        transcript = parakeet_data.get("text", "").strip()
        if not transcript:
            self._error(200, "Empty transcript")
            return

        # Step 2: enhance via Ollama
        import urllib.request
        prompt = f"Clean up this voice transcript. Fix punctuation, grammar, and remove filler words. Keep the original meaning. Only output the cleaned text.\n\n{transcript}"
        payload = json.dumps({
            "model": OLLAMA_MODEL,
            "messages": [{"role": "user", "content": prompt}],
            "stream": False,
        }).encode()
        try:
            req = urllib.request.Request(
                f"{OLLAMA_URL}/api/chat", data=payload,
                headers={"Content-Type": "application/json"},
            )
            with urllib.request.urlopen(req, timeout=300) as resp:
                data = json.loads(resp.read())
            text = data.get("message", {}).get("content", "").strip()
        except Exception as e:
            print(f"Ollama error: {e}", file=sys.stderr)
            self._error(502, "Ollama enhancement failed")
            return

        if not text:
            self._error(200, "Empty result")
            return

        # Send to ntfy (best-effort)
        try:
            token = read_token()
            subprocess.run(
                [CURL, "-sf", "--max-time", "10",
                 "-H", f"Authorization: Bearer {token}",
                 "-H", "Title: Voice Note",
                 "-H", "Tags: studio_microphone",
                 "-d", text,
                 NTFY_URL],
                capture_output=True, timeout=15,
            )
        except (subprocess.TimeoutExpired, OSError):
            pass

        response_body = json.dumps({"text": text}).encode()
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(response_body)))
        self.end_headers()
        self.wfile.write(response_body)

    def _handle_whisper_en(self):
        """Transcribe via whisper-server with small.en model (English-only)."""
        if not WHISPER_EN_URL:
            self._error(503, "Whisper English-only server not configured")
            return

        audio_data, _language, _prompt = self._read_upload()
        if audio_data is None:
            return
        if self._looks_like_image(audio_data):
            self._error(400, "Images are only supported on the whisper + LLM endpoint")
            return

        with tempfile.NamedTemporaryFile(suffix=".audio", delete=False) as f:
            f.write(audio_data)
            tmp_path = f.name

        try:
            curl_cmd = [
                CURL, "-sf",
                "-F", f"file=@{tmp_path}",
                "-F", "response_format=json",
                "-F", "language=en",
            ]
            curl_cmd.append(WHISPER_EN_URL)
            result = subprocess.run(curl_cmd, capture_output=True, text=True, timeout=60)
        finally:
            os.unlink(tmp_path)

        if result.returncode != 0:
            print(f"whisper-en error: {result.stderr[:200]}", file=sys.stderr)
            self._error(502, "Transcription failed (whisper.en)")
            return

        try:
            data = json.loads(result.stdout)
        except json.JSONDecodeError:
            self._error(502, "Invalid response from whisper-en")
            return

        transcript = data.get("text", "").strip()
        if not transcript:
            self._error(200, "Empty transcript")
            return

        response_body = json.dumps({"text": transcript}).encode()
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(response_body)))
        self.end_headers()
        self.wfile.write(response_body)

    def _handle_whisper_cpp(self):
        """Transcribe via whisper-server (HTTP)."""
        audio_data, language, _prompt = self._read_upload()
        if audio_data is None:
            return
        if self._looks_like_image(audio_data):
            self._error(400, "Images are only supported on the whisper + LLM endpoint")
            return

        # Save to temp file, then forward to whisper-server via curl
        with tempfile.NamedTemporaryFile(suffix=".audio", delete=False) as f:
            f.write(audio_data)
            tmp_path = f.name

        try:
            curl_cmd = [
                CURL, "-sf",
                "-F", f"file=@{tmp_path}",
                "-F", "response_format=json",
            ]
            if language and language != "auto":
                curl_cmd += ["-F", f"language={language}"]
            curl_cmd.append(WHISPER_URL)
            result = subprocess.run(curl_cmd, capture_output=True, text=True, timeout=60)
        finally:
            os.unlink(tmp_path)

        if result.returncode != 0:
            print(f"whisper-server error: {result.stderr[:200]}", file=sys.stderr)
            self._error(502, "Transcription failed")
            return

        try:
            data = json.loads(result.stdout)
        except json.JSONDecodeError:
            self._error(502, "Invalid response from whisper-server")
            return

        transcript = data.get("text", "").strip()
        if not transcript:
            self._error(200, "Empty transcript")
            return

        # Send to ntfy (best-effort)
        try:
            token = read_token()
            subprocess.run(
                [CURL, "-sf", "--max-time", "10",
                 "-H", f"Authorization: Bearer {token}",
                 "-H", "Title: Voice Note",
                 "-H", "Tags: studio_microphone",
                 "-d", transcript,
                 NTFY_URL],
                capture_output=True, timeout=15,
            )
        except (subprocess.TimeoutExpired, OSError):
            pass

        response_body = json.dumps({"text": transcript}).encode()
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(response_body)))
        self.end_headers()
        self.wfile.write(response_body)

    def do_GET(self):
        if self.path == "/health":
            try:
                self.send_response(200)
                self.end_headers()
                self.wfile.write(b"ok")
            except (BrokenPipeError, ConnectionResetError):
                pass
            return
        # Serve upload form
        self.send_response(200)
        self.send_header("Content-Type", "text/html")
        self.end_headers()
        self.wfile.write(UPLOAD_PAGE.encode())

    @staticmethod
    def _looks_like_image(data):
        return (
            data[:3] == b'\xff\xd8\xff'
            or data[:8] == b'\x89PNG\r\n\x1a\n'
            or data[:6] in (b'GIF87a', b'GIF89a')
            or (len(data) >= 12 and data[:4] == b'RIFF' and data[8:12] == b'WEBP')
        )

    @staticmethod
    def _parse_multipart(content_type, body):
        """Extract file and language from multipart/form-data using email.parser."""
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
            elif name == "language":
                fields["language"] = payload.decode().strip()
            elif name == "prompt":
                fields["prompt"] = payload.decode().strip()
        # Fallback: if no named file part, use first binary payload
        if "file" not in fields:
            for part in msg.walk():
                payload = part.get_payload(decode=True)
                if payload and len(payload) > 100:
                    fields["file"] = payload
                    break
        return fields

    def _error(self, code, message):
        body = json.dumps({"error": message}).encode()
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
    print(f"Voice assistant listening on {LISTEN_HOST}:{LISTEN_PORT}", file=sys.stderr)
    server.serve_forever()
