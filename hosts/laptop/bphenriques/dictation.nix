{
  # Only the NVIDIA driver is visible to Voxtype, so it streams on the RTX 4060: the 780M re-transcribes a full window in
  # 3.4s against the 4060's 1.0s (large-v3-turbo, measured 2026-11-13).
  services.voxtype.environment.VK_LOADER_DRIVERS_SELECT = "*nvidia*";
}
