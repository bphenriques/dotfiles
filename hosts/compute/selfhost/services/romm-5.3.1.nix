# Temporary: delete this file and its import once nixpkgs ships romm >= 5.3.1.
#
# nixpkgs is on 5.2.0; 5.3.1 is the first release shipping the js-dos player. Beyond the sources it
# needs rq >= 2.12, whose `rq cron` replaced the rq-scheduler daemon upstream dropped, and the two
# task wrappers rebuilt around that command and the `scans` queue scans moved to.
{ config, ... }:
{
  nixpkgs.overlays = [
    (_final: prev:
      let
        inherit (prev) lib;

        python3 = prev.python3.override {
          packageOverrides = _: pyPrev: {
            rq = pyPrev.rq.overrideAttrs {
              version = "2.12.0";
              src = prev.fetchFromGitHub {
                owner = "rq";
                repo = "rq";
                tag = "v2.12";
                hash = "sha256-EGgOeATfuRONPxfbgBqrgK8tk4Ehg9El7bTGOVX4cLY=";
              };
            };
          };
        };
      in
      {
        romm = (prev.romm.override { inherit python3; }).overrideAttrs (finalAttrs: prevAttrs: {
          version = "5.3.1";

          src = prev.fetchFromGitHub {
            owner = "rommapp";
            repo = "romm";
            tag = finalAttrs.version;
            hash = "sha256-ijfp4L4GdGbr4FcBo83xVnGEksXawrkK3rYe8/Is+NU=";
          };

          passthru = prevAttrs.passthru // {
            frontend = prevAttrs.passthru.frontend.overrideAttrs {
              # buildNpmPackage resolves npmDeps from npmDepsHash before overrideAttrs runs, so the
              # dependency fetch has to be replaced rather than re-hashed.
              npmDeps = prev.fetchNpmDeps {
                name = "romm-frontend-${finalAttrs.version}-npm-deps";
                src = "${finalAttrs.src}/frontend";
                hash = "sha256-x8Chw4nMoyq0M+m4XhIgqNu8Lgr4U7w38V7CYrd1zK4=";
              };
            };
          };

          postInstall =
            let
              inherit (finalAttrs.passthru) pythonEnv;
              backend = "$out/share/romm/backend";
            in
            ''
              rm $out/bin/romm-scheduler $out/bin/romm-worker

              makeWrapper ${pythonEnv}/bin/rq $out/bin/romm-scheduler \
                --chdir ${backend} \
                --set PYTHONPATH ${backend} \
                --prefix PATH : ${lib.makeBinPath [ prev.p7zip ]} \
                --add-flags "cron --path ${backend} tasks.cron_config"

              # Upstream gives the `scans` queue a worker of its own; on one worker it goes last so an
              # hours-long scan is not picked up ahead of the short jobs. --with-scheduler releases the
              # delayed jobs the watcher enqueues.
              makeWrapper ${pythonEnv}/bin/rq $out/bin/romm-worker \
                --chdir ${backend} \
                --set PYTHONPATH ${backend} \
                --prefix PATH : ${lib.makeBinPath [ prev.p7zip ]} \
                --add-flags "worker --path ${backend} --worker-class handler.rq_worker.RomMWorker --with-scheduler high default low scans"
            '';
        });
      })
  ];

  # The module's cross-origin isolation map predates the js-dos player, whose DOSBox-X backend is a
  # threaded build and refuses to start without SharedArrayBuffer. A regex location beats the
  # module's `/` prefix, so the headers land on the player URL without redefining its map.
  services.nginx.virtualHosts.${config.services.romm.nginx.virtualHost}.locations."~ ^/rom/.*/jsdos$" = {
    tryFiles = "$uri $uri/ /index.html";
    extraConfig = ''
      add_header Cache-Control "no-cache";
      add_header Cross-Origin-Embedder-Policy "require-corp";
      add_header Cross-Origin-Opener-Policy "same-origin";
    '';
  };
}
