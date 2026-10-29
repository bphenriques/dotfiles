# Drop once nixpkgs' romm catches up with 5.4: it still wraps the rq-scheduler daemon upstream dropped
# for `rq cron`, runs one worker without the `scans` queue or --with-scheduler, and its Python env
# lacks deps 5.4 imports.
_final: prev: {
  romm = prev.romm.overrideAttrs (finalAttrs: prevAttrs: {
    passthru = prevAttrs.passthru // {
      pythonEnv = prevAttrs.passthru.pythonEnv.override (old: {
        extraLibs = old.extraLibs ++ (with old.python.pkgs; [ apprise croniter httpx2 ]);
      });
    };

    postInstall =
      let
        inherit (finalAttrs.passthru) pythonEnv;
        backend = "$out/share/romm/backend";
      in
      (prevAttrs.postInstall or "")
      + ''
        rm $out/bin/romm-scheduler $out/bin/romm-worker

        makeWrapper ${pythonEnv}/bin/rq $out/bin/romm-scheduler \
          --chdir ${backend} \
          --set PYTHONPATH ${backend} \
          --prefix PATH : ${prev.lib.makeBinPath [ prev.p7zip ]} \
          --add-flags "cron --path ${backend} --connection-class handler.redis_handler.RomMRedis tasks.cron_config"

        # Upstream gives the `scans` queue a worker of its own; on one worker it goes last so an
        # hours-long scan is not picked up ahead of the short jobs. --with-scheduler releases the
        # delayed jobs the watcher enqueues.
        makeWrapper ${pythonEnv}/bin/rq $out/bin/romm-worker \
          --chdir ${backend} \
          --set PYTHONPATH ${backend} \
          --prefix PATH : ${prev.lib.makeBinPath [ prev.p7zip ]} \
          --add-flags "worker --path ${backend} --worker-class handler.rq_worker.RomMWorker --connection-class handler.redis_handler.RomMRedis --with-scheduler high default low scans"
      '';
  });
}
