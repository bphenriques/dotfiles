{
  # Basic
  "browser.startup.homepage" = "about:blank";                   # Peaceful.
  "browser.warnOnQuit" = false;                                 # Yes, I am aware of my 100 tabs.
  "browser.shell.checkDefaultBrowser" = false;                  # Let me handle the default.
  "browser.aboutConfig.showWarning" = false;                    # I know what I am doing, erm, most of the times...
  "services.sync.engine.passwords" = false;                     # I manage my passwords separately.
  "signon.rememberSignons" = false;                             # Do not ask me to remember any password...
  "browser.urlbar.trimURLs" = false;                            # Always show http/https
  "browser.tabs.firefox-view" = false;                          # I rarely have the need to sync tabs across devices. Plus, won't use a firefox account.
  "services.sync.prefs.sync.layout.spellcheckDefault" = false;  # Do not spell-check
  "layout.spellcheckDefault" = 0;                               # Do not spell-check
  "browser.translations.neverTranslateLanguages" = "pt";        # Do not prompt me for languages I do know.
  "browser.startup.homepage_override.mstone" = "ignore";        # Do not show the latest changes whenever there is an update
  "browser.disableResetPrompt" = true;                          # Never seen this myself
  "browser.download.alwaysOpenPanel" = false;                   # Do not automatically open the download panel.
  "browser.download.useDownloadDir" = false;                    # Let me decide where to download the files.
  "identity.fxaccounts.enabled" = false;                        # Disable Firefox Sync.
  "browser.toolbars.bookmarks.showInPrivateBrowsing" = true;    # Show bookmarks in private tabs as well.
  "browser.toolbars.bookmarks.visibility" = "always";           # Always show the toolbar.

  # Extra Security
  "privacy.globalprivacycontrol.functionality.enabled" = true;  # Private as much as possible.
  "dom.security.https_only_mode" = true;                        # Use HTTPS where possibe.
  "privacy.trackingprotection.enabled" = true;                  # Do not track me.
  "privacy.donottrackheader.enabled" = true;                    # Do not track me.

  # I like boring new tab pages.
  "browser.newtabpage.enabled" = false;
  "browser.newtabpage.introShown" = false;
  "browser.newtabpage.pinned" = [];
  "browser.newtabpage.enhanced" = false;
  "browser.newtabpage.activity-stream.improvesearch.topSiteSearchShortcuts" = false;

  ## Disable Pocket
  "extensions.pocket.enabled" = false;
  "browser.newtabpage.activity-stream.section.highlights.includePocket" = false;

  # Unsure how portable this is if I change the extensions.. BUT.. this automatically sets the toolbar to my liking.
  "browser.uiCustomization.state" = ''{"placements":{"widget-overflow-fixed-list":[],"unified-extensions-area":["_74145f27-f039-47ce-a470-a662b129930a_-browser-action","_testpilot-containers-browser-action","amptra_keepa_com-browser-action","_b7f9d2cd-d772-4302-8c3f-eb941af36f76_-browser-action","vim-vixen_i-beam_org-browser-action"],"nav-bar":["back-button","forward-button","stop-reload-button","urlbar-container","save-to-pocket-button","downloads-button","fxa-toolbar-menu-button","unified-extensions-button","reset-pbm-toolbar-button","_446900e4-71c2-419f-a6a7-df9c091e268b_-browser-action","ublock0_raymondhill_net-browser-action"],"TabsToolbar":["firefox-view-button","tabbrowser-tabs","new-tab-button","alltabs-button"],"PersonalToolbar":["personal-bookmarks"]},"seen":["developer-button","_446900e4-71c2-419f-a6a7-df9c091e268b_-browser-action","_74145f27-f039-47ce-a470-a662b129930a_-browser-action","_testpilot-containers-browser-action","amptra_keepa_com-browser-action","_b7f9d2cd-d772-4302-8c3f-eb941af36f76_-browser-action","ublock0_raymondhill_net-browser-action","vim-vixen_i-beam_org-browser-action"],"dirtyAreaCache":["nav-bar","PersonalToolbar","unified-extensions-area","TabsToolbar"],"currentVersion":20,"newElementCount":4}'';
}
