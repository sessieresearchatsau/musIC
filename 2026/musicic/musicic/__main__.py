"""Entry point: start the local server and open a browser."""
from __future__ import annotations

import argparse
import threading
import webbrowser


def main() -> None:
    ap = argparse.ArgumentParser(prog="musicic", description="Music <-> set workbench")
    ap.add_argument("--port", type=int, default=8730)
    ap.add_argument("--host", default="127.0.0.1")
    ap.add_argument("--no-browser", action="store_true")
    args = ap.parse_args()

    import uvicorn
    url = f"http://{args.host}:{args.port}/"
    if not args.no_browser:
        threading.Timer(1.0, lambda: webbrowser.open(url)).start()
    print(f"musIC -> {url}")
    uvicorn.run("musicic.server.app:app", host=args.host, port=args.port,
                log_level="warning")


if __name__ == "__main__":
    main()
