{
  "browser.selfsupport.url" = ""; # Disable Heartbeat Userrating: https://wiki.mozilla.org/Advocacy/heartbeat

  # No Sponsor websites
  "services.sync.prefs.sync.browser.newtabpage.activity-stream.showSponsoredTopSite" = false;
  "browser.newtabpage.activity-stream.showSponsored" = false;
  "services.sync.prefs.sync.browser.newtabpage.activity-stream.showSponsored" = false;
  "browser.newtabpage.activity-stream.showSponsoredTopSites" = false;

  # Disable telemetry: https://support.mozilla.org/kb/share-telemetry-data-mozilla-help-improve-firefox)
  "toolkit.telemetry.enabled" = false;
  "toolkit.telemetry.archive.enabled" = false;
  "toolkit.telemetry.rejected" = true;
  "toolkit.telemetry.unified" = false;
  "toolkit.telemetry.unifiedIsOptIn" = false;
  "toolkit.telemetry.prompted" = 2;
  "toolkit.telemetry.server" = "";
  "toolkit.telemetry.cachedClientID" = "";
  "toolkit.telemetry.newProfilePing.enabled" = false;
  "toolkit.telemetry.shutdownPingSender.enabled" = false;
  "toolkit.telemetry.updatePing.enabled" = false;
  "toolkit.telemetry.bhrPing.enabled" = false;
  "toolkit.telemetry.firstShutdownPing.enabled" = false;
  "toolkit.telemetry.hybridContent.enabled" = false;
  "toolkit.telemetry.reportingpolicy.firstRun" = false;

  # Disable health report: https://www.mozilla.org/privacy/firefox/#health-report
  "datareporting.healthreport.uploadEnabled" = false;
  "datareporting.policy.dataSubmissionEnabled" = false;
  "datareporting.healthreport.service.enabled" = false;

  # Disable shield studies that allows firefox to install experimental addons:
  "app.normandy.enabled" = false;
  "app.normandy.api_url" = "";
  "app.shield.optoutstudies.enabled" = false;
  "extensions.shield-recipe-client.enabled" = false;
  "extensions.shield-recipe-client.api_url" = "";

  # Disable experiments: https://wiki.mozilla.org/Telemetry/Experiments
  "experiments.enabled" = false;

  # Disable Crash Reports: https://www.mozilla.org/privacy/firefox/#crash-reporter
  "breakpad.reportURL" = "";
  "browser.tabs.crashReporting.sendReport" = false;
  "browser.crashReports.unsubmittedCheck.enabled" = false;
  "browser.crashReports.unsubmittedCheck.autoSubmit" = false;
  "browser.crashReports.unsubmittedCheck.autoSubmit2" = false;

  # Opt out metadata updates: https://blog.mozilla.org/addons/how-to-opt-out-of-add-on-metadata-updates/
  "extensions.getAddons.cache.enabled" = false;

  # Disable google safebrowsing: http://electroholiker.de/?p=1594
  "browser.safebrowsing.enabled" = false;
  "browser.safebrowsing.downloads.remote.url" = "";
  "browser.safebrowsing.phishing.enabled" = false;
  "browser.safebrowsing.blockedURIs.enabled" = false;
  "browser.safebrowsing.downloads.enabled" = false;
  "browser.safebrowsing.downloads.remote.enabled" = false;
  "browser.safebrowsing.appRepURL" = "";
  "browser.safebrowsing.malware.enabled" = false;

  # I use my own server for this.
  "network.trr.mode" = 5;

  # Disable about:addons' Get Add-ons panel. The start-page with recommended addons uses google analytics.
  "extensions.getAddons.showPane" = false;
  "extensions.webservice.discoverURL" = "";
  "browser.urlbar.groupLabels.enabled" = false;
  "browser.urlbar.quicksuggest.enabled" = false;
  "browser.newtabpage.activity-stream.feeds.recommendationprovider" = false;
  "extensions.htmlaboutaddons.recommendations.enabled" = false;
 }
