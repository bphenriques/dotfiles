# Files under llama-swap's models directory, by model id, pinned by revision and sha256.
let
  hf = repo: rev: files: map (path: {
    inherit path;
    sha256 = files.${path};
    url = "https://huggingface.co/${repo}/resolve/${rev}/${path}";
  }) (builtins.attrNames files);

  # Gated behind a click-through on Hugging Face, so placed by hand (see README "Models").
  manual = files: map (path: { inherit path; sha256 = files.${path}; url = null; }) (builtins.attrNames files);
in
{
  # Gufo has kernels for this exact file set only, not other quants of the same model.
  "qwen3.8-flash-next" = hf "unsloth/Qwen3.8-Flash-Next-GGUF" "d96201194f103c906994f283f504209dc8d2a10c" {
    "UD-Q4_K_XL/Qwen3.8-Flash-Next-UD-Q4_K_XL-00001-of-00004.gguf" = "acc44040f74d0e55fb89c3967056cce5dbda1bcc52e909c3ba6fc72b5e9c0858";
    "UD-Q4_K_XL/Qwen3.8-Flash-Next-UD-Q4_K_XL-00002-of-00004.gguf" = "45792ae869072d8229384c51c7c73dd959e191505252b41dd71852d9746f5ce4";
    "UD-Q4_K_XL/Qwen3.8-Flash-Next-UD-Q4_K_XL-00003-of-00004.gguf" = "8c76e79d444b16a663d9c8db54e585ed3b1da13eebd2619c37b0c355da803c82";
    "UD-Q4_K_XL/Qwen3.8-Flash-Next-UD-Q4_K_XL-00004-of-00004.gguf" = "5de8b5dbf89409acc39468decafd4a4a95b13c29d88a14b0356107897829ec36";
    "MTP/mtp-Qwen3.8-Flash-Next-shared-Q8_0.gguf" = "5ff54097406a905cf3a724c709124ceb0e3e10235ee862298969e91c96fa96e6";
    "mmproj-BF16.gguf" = "2e788f8c511d8093c7b43cb87b2fd7e14228340318057f8fb20c86df2efe2355";
  };

  "qwen3.8-27b-uncensored" =
    manual {
      "Qwen3.8-27B-Uncensored-Q4_K_M.gguf" = "3445102e9cde5d562508642c100a2f5ac3368a5a3f748442811d7a95daee3bec";
      "mmproj-Qwen3.8-27B-Uncensored-f16.gguf" = "add205b7bfdb3f71f6da36b0a82aa20928dd829a920878c602628cdfbebc5288";
    }
    ++ hf "z-lab/Qwen3.8-27B-DFlash2-GGUF" "2d9571f8ce46e151f61c6499c99dee6079e1d610" {
      "Qwen3.8-27B-DFlash2-Q4_K_M.gguf" = "1a25c56858e1ebe93f2718ac1d49d1151f9323325c1bbfd6209370f4db131ebd";
    };

  "qwen-image-2.1-turbo" = hf "Qwen/Qwen-Image-2.1-Turbo" "fc13ba1986fcb905854dc8de2f52eee395f49b5b" {
    "model_index.json" = "97e2febeb19d93cb41fa44807cde8bd42757950b5dffeacdb0331fb2d390cf20";
    "processor/chat_template.jinja" = "3636d0f0bd6bef02654cdffdc447b79cb2cef8ab02cc75267345946291a489e4";
    "processor/processor_config.json" = "14932921ca485d458a04dafd8069fbb0a4505622a48208d19ed247115801385b";
    "processor/tokenizer_config.json" = "fd42a0b35a8d13cd473e88e310b50cbf1bccaa9eb60ed0fd873af5f8f0c59307";
    "processor/tokenizer.json" = "be75606093db2094d7cd20f3c2f385c212750648bd6ea4fb2bf507a6a4c55506";
    "scheduler/scheduler_config.json" = "16c948c71f9152a34cce6e3a2f309e97ad2dca32beecbe5e2fd3a7925b7eb4bb";
    "text_encoder/config.json" = "89050cba6b66c4f68f52305b54fc37efe28d95232246b94aa8dc0cfa36f3b734";
    "text_encoder/generation_config.json" = "d04d0c6fdd1dc056cf68178425a3ecf17a0b9e848cf1a8378ad31c623263472c";
    "text_encoder/model.safetensors" = "a1eee8233f3063466dd3b06e857b0a2cf13a77c543908374771d51a546beb2e0";
    "transformer/config.json" = "2c567038ca190824728844b8d06d94ae02360e668707afd9734e789a7eb58ce3";
    "transformer/diffusion_pytorch_model-00001-of-00002.safetensors" = "6cccd922767f01694461bdcf1f34ea7b771f3442e4d5e338ea9aa55277431cc0";
    "transformer/diffusion_pytorch_model-00002-of-00002.safetensors" = "69f53ebb063d2f606bdaff7e22e0e2144f28d978d73dc75e6e4293fc5982eab5";
    "transformer/diffusion_pytorch_model.safetensors.index.json" = "17987f6623b1c814d0ef55a137d99142b7b3b040eb1bf241b5575dd35af803a2";
    "vae/config.json" = "42bfdec412459bbb7bc38a0617ab70edcfc873f84ec47e0c7d01c53ba09885a5";
    "vae/diffusion_pytorch_model.safetensors" = "71879ffd5321e6d10c3c87513e2b474b1252efa7f3dec2969214a9bf06a6dd5c";
  };

  "embeddinggemma-2" = hf "google/embeddinggemma-2" "914f7f89142e33e77833254d9c9b90c3cef7303b" {
    "1_Pooling/config.json" = "8759bdf7c77efc7df7723f64856a593c8943b71ee38baf2a88771fbaf78438f9";
    "2_Normalize/config.json" = "cdb09dfca347a56aa2d691744e38d5ad3c7cbc2834e7181272b9a15328b82524";
    "chat_template.jinja" = "4b852efc0b9960283e735363331e6f325b33bc74bdbaa076f595bc4e9b94d85e";
    "config.json" = "b8f1e9931b57fbc054acdb445c41765d55b0074c58d145fa82839941ad1b5bb3";
    "config_sentence_transformers.json" = "031e56a498d33c349ab489a21885bcfe25b4fcba841149dc99e1e90d4a7c28f5";
    "model.safetensors" = "197a32965d4b1105faf060417baa899e193fb73cd401f42ec9295234d5553d79";
    "modules.json" = "3d02572a0455b832de67fb8e63a54981bc7e8b46e337c95e917bd8122a533bfd";
    "preprocessor_config.json" = "ea2ae257e901064abdd98dceb19f2b0da06af600bed15e0f99f5c85c37ee9d78";
    "processor_config.json" = "168f6a08522f3ce5dea596d94d003af2fd691742d4f41fe1f9d8cce76bfbf69c";
    "sentence_bert_config.json" = "b1bcd9f2dce3ae863b359e87d0710b5dbc3314a59ecb4e2f97c7778fc8e4b228";
    "tokenizer_config.json" = "17bd5d6e9364ca49a534e1502076593317c298d4a663623091ed45388f004874";
    "tokenizer.json" = "4d777ef5bdc1aa36227abdfb77c3e49e7b9c892d16e1b6bda41c393504828be4";
    "tokenizer.model" = "e594c8a90eb08d8bda498ff4747977dc827ae0c3c56b5c0d41a605a22d02ef03";
  };
}
