"""Minimal Wyoming STT client: send audio file, print JSON transcript."""
import asyncio
import json
import sys
import wave

from wyoming.asr import Transcribe, Transcript
from wyoming.audio import AudioChunk, AudioStart, AudioStop
from wyoming.client import AsyncTcpClient


async def transcribe(host, port, audio_path, language=None):
    import subprocess, tempfile, os
    wav_path = tempfile.mktemp(suffix=".wav")
    try:
        subprocess.run(
            ["ffmpeg", "-y", "-i", audio_path, "-ar", "16000", "-ac", "1", "-f", "wav", wav_path],
            capture_output=True, check=True, timeout=30,
        )

        with wave.open(wav_path, "rb") as w:
            rate = w.getframerate()
            width = w.getsampwidth()
            channels = w.getnchannels()
            audio_data = w.readframes(w.getnframes())
    finally:
        if os.path.exists(wav_path):
            os.unlink(wav_path)

    async with AsyncTcpClient(host, port) as client:
        await client.write_event(Transcribe(language=language).event())
        await client.write_event(AudioStart(rate=rate, width=width, channels=channels).event())

        chunk_size = rate * width * channels  # 1 second chunks
        for i in range(0, len(audio_data), chunk_size):
            await client.write_event(
                AudioChunk(rate=rate, width=width, channels=channels, audio=audio_data[i:i+chunk_size]).event()
            )

        await client.write_event(AudioStop().event())

        while True:
            event = await client.read_event()
            if event is None:
                break
            if Transcript.is_type(event.type):
                return Transcript.from_event(event).text

    return ""


if __name__ == "__main__":
    host = sys.argv[1]
    port = int(sys.argv[2])
    audio_path = sys.argv[3]
    language = sys.argv[4] if len(sys.argv) > 4 else None

    text = asyncio.run(transcribe(host, port, audio_path, language))
    print(json.dumps({"text": text}))
