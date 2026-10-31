# Alfred's definition for hermes-agent; hosts/ai/microvm/guests/agent-alfred-vm deploys it. See ./README.md.
{ pkgs, lib, inputs, facts }:
let
  inherit (facts) vaultRoot recipesRoot recipesUrl;

  tools = import ./tools { inherit pkgs lib facts; };
  inherit (tools) meals drafts;

  # Copied, not linked: skill_view and `find -type f` reject a file that resolves outside the skill's folder.
  withDrafts = name: pkgs.runCommand name { } ''
    install -Dm444 ${./skills/${name}/SKILL.md} $out/SKILL.md
    install -Dm555 ${lib.getExe drafts} $out/scripts/drafts
  '';

  # The only skills offered besides the essential `hermes-agent`.
  skills = pkgs.linkFarm "alfred-skills" {
    youtube-content = "${inputs.hermes-agent}/skills/media/youtube-content";
    pdf = "${inputs.hermes-agent}/skills/productivity/pdf";
    meal-planning = ./skills/meal-planning;
    recipe-new = withDrafts "recipe-new";
    recipe-import = withDrafts "recipe-import";
    recipe-aisle = withDrafts "recipe-aisle";
  };

  mcpServers = {
    meals = { command = lib.getExe meals; args = [ "serve" ]; };

    # Read-only: new recipes reach the library through `drafts promote`, edits through `patch`.
    cook = {
      command = lib.getExe pkgs.cook-mcp;
      env.COOK_RECIPES_DIR = "${recipesRoot}/library";
      tools.include = [ "search_recipes" "read_recipe" ];
    };
    fetch.command = lib.getExe pkgs.mcp-server-fetch;
    time.command = lib.getExe pkgs.mcp-server-time;

    # Fetched on first boot, see TODO.md.
    vault = {
      command = "${pkgs.nodejs}/bin/npx";
      args = [ "-y" "@bitbonsai/mcpvault@0.16.0" vaultRoot ];
      tools.include = [
        "search_notes" "read_note" "read_multiple_notes" "write_note" "patch_note" "move_note" "list_directory"
        "get_frontmatter" "update_frontmatter" "manage_tags"
      ];
    };
  };
in
{
  inherit (tools) cookidoo meals drafts;

  soul = builtins.readFile ./SOUL.md;
  environment = builtins.replaceStrings [ "@vaultRoot@" "@recipesRoot@" "@recipesUrl@" ] [ vaultRoot recipesRoot recipesUrl ] (builtins.readFile ./environment.md);

  # nodejs for mcpvault's `#!/usr/bin/env node`, cook-cli for the recipes AGENTS.md, the rest for the pdf and
  # youtube-content skills. Recipes, videos and photos go through drafts.
  packages = [
    pkgs.nodejs pkgs.cook-cli pkgs.poppler-utils
    (pkgs.python3.withPackages (ps: with ps; [
      youtube-transcript-api pypdf pdfplumber reportlab pymupdf pymupdf4llm pypdfium2 pillow
    ]))
  ];

  inherit mcpServers;

  settings = {
    # "auto" hides MCP tools behind a search indirection, and the model then drops their required arguments.
    tools.tool_search.enabled = "off";

    # Otherwise hermes adds four prompt/resource wrapper tools per server, outside the allowlists.
    mcp_servers = lib.mapAttrs (_: _: { tools = { resources = false; prompts = false; }; }) mcpServers;

    # `search` is web_search alone; pages go through the `fetch` MCP.
    platform_toolsets.api_server = [ "memory" "session_search" "todo" "vision" "cronjob" "terminal" "file" "skills" "search" ];

    skills = {
      # Read-only, so agent-created skills land in HERMES_HOME/skills.
      external_dirs = [ "${skills}" ];
      # The local model misjudges what it learned, so its skill writes wait for `/skills approve`.
      write_approval = true;
    };

    # Holds even with approvals off, under the default `smart` mode.
    approvals.deny = [
      "git push --force*"
      "git push *--force*"
      "git push *--delete*"
      "*curl*|*sh*"
      "rm -rf ${vaultRoot}*"
      "rm -rf ${recipesRoot}*"
    ];
  };
}
