# Calendar integration

Parked 2026-09-06. Nothing built.

## Goal

hermes in `agent-vm` reads and edits a Google calendar shared with a partner. Events the bot creates
or changes must land on her existing shared calendar with **no subscription step on her side**, and
neither personal Google account may hand credentials to the bot.

## Outcome

Use a community MCP server inside the guest. Do not build a custom syncer.

- `@cocal/google-calendar-mcp` (nspady/google-calendar-mcp, MIT, 1181 stars, active 2026-09-02),
  npx-pinned, same shape as the `vault` entry in `hosts/guests/agent-vm/services/hermes-agent.nix`.
- **Identity boundary:** a dedicated bot Gmail account, invited to the shared calendar with one ACL
  entry ("Make changes to events"). That entry is the bot's entire privilege.
- **Capability boundary:** `ENABLED_TOOLS` allowlist. Grant `list-calendars, list-events, search-events, get-event, get-freebusy, get-current-time, create-event, update-event`. Withhold
  `delete-event`, `respond-to-event`, `manage-accounts`. Deletion stays a human action.
- Credentials are the guest's own secret, so its own sops file.

## Why not the custom syncer

The original design was snapshot + spool: compute holds the credential, pulls to RO virtiofs, the bot
writes intent to a RW spool, a `.path` unit applies it, and an ownership gate refuses to touch events
the bot did not create. Roughly 250-350 lines of Python, 3 to 5 evenings, plus permanent ownership of
the OAuth error surface.

Killed by one requirement: **the bot should be able to edit hand-created events.** That dissolves the
ownership gate, which was the only thing the custom code bought. Without it the syncer is strictly
worse than the MCP server: more code, fewer features, identical security posture.

## Measured, do not re-derive

- **The seal blocks LAN, not internet.** `modules/nixos/microvm/host.nix:77` drops
  guest to RFC1918, `:173` drops guest to host. The guest reaches `googleapis.com` fine, so an MCP
  server holding Google credentials runs *inside* the guest. Got this wrong once; it is what makes
  the community tooling usable at all.
- **No Calendar OAuth scope narrows to a single calendar.** `calendar.app.created` is narrowest and
  only covers app-created calendars. Granularity comes from *which account holds the token*, never
  from the scope string. This is why the separate identity is load-bearing.
- **There is no ICS MCP ecosystem.** `Omar-V2/mcp-ical` (329 stars) is macOS EventKit, not ICS files,
  stale since 2025-04-21. Every genuine ICS-file MCP server has 3 stars or fewer. nixpkgs has no
  calendar MCP at all, only `mcp-server-{fetch,filesystem,git,memory,sequential-thinking,time}`.
- **"Conflict resolution" is not inherent to the problem.** It exists only with two writable copies.
  Google as the single source of truth removes the concern.
- **Read-only needs no OAuth.** A calendar's secret ICS address is a plain fetch: no GCP project, no
  consent screen, no refresh token, nothing to expire. If scope is ever cut back to read-only, that
  is a curl in a timer and `hosts/compute/microvm/agent-vm-vault.nix` is the template. Untested:
  how fresh that feed is. The widely quoted 8-24h figures describe Google polling *external* feeds,
  the opposite direction, so measure before trusting either way.
- Calendar API v3 is free at 1,000,000 req/day; a 5-minute poll is 288/day. Charges stated to begin
  later in 2026 only for exceeding per-minute quotas. Confirm the billing-account note at setup.
- "Testing" OAuth status issues 7-day refresh tokens. Publishing to Production, even unverified under
  100 users, removes that.

## Rejected

| Option                                                 | Why not                                                                                                                                                                                                                                                        |
| ------------------------------------------------------ | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `taylorwilsdon/google_workspace_mcp` (3120 stars, MIT) | `--read-only` is binary, cannot express "update yes, delete no". Whole-Workspace scope is wider than needed. Reconsider if the ask grows past calendar.                                                                                                        |
| Keeper.sh (1277 stars, AGPL)                           | Needs Docker + Postgres + Redis + a public hostname. Its `@keeper.sh` UID ownership marker guards only the cleanup sweep, not the MCP write tools, so it would not have given the constraint anyway. Container-only, so dotfiles + overlays, not selfhost-nix. |
| vdirsyncer (nixpkgs 0.20.0)                            | Faithful mirror with no ownership concept, so a bot bug propagates deletions to her events. Needs full `auth/calendar`. Worse than the status quo.                                                                                                             |
| Radicale as a bot-side calendar                        | Falls off the critical path once Google is the source of truth, and it reintroduces the subscription step the whole thing exists to avoid.                                                                                                                     |
| Reuse the personal Google account                      | Scopes are account-wide. Collapses identity and ACL into one boundary, and bot events become indistinguishable from hand-made ones.                                                                                                                            |

## Open before building

1. **Is `ENABLED_TOOLS` enforced server-side, or does it only trim the advertised manifest?**
   Boundary versus hint. Read the source before relying on it.
2. Accept explicitly that this puts a live Google write credential in the guest. Blast radius is one
   calendar, bounded by the ACL rather than by code. That is the price of dropping the custom gate.
3. npx-at-boot pulls from the npm registry. Pin the version the way `@bitbonsai/mcpvault@0.12.4` is
   pinned; same failure class as the `node`-on-PATH issue.
4. **Whether the value justifies any of it.** Reading is trivial on a phone. The bot only clearly
   beats the app on aggregative queries (free/busy, clash detection, digests, month-level analysis)
   and bulk creation, not on single-event edits.

## If revisited

Constraining writes without an ownership gate: field-scoped writes (allow `start`, `end`, `location`,
`description`, `reminders` on user events; refuse `summary`, `attendees` and delete) plus an
append-only journal of the prior event JSON before each edit. **The journal matters more than the
gate**, since it makes mistakes reversible rather than merely unlikely.

**Recurrence is the long-tail bug source**: RRULE, EXDATE, all-day versus timed, DST, instance versus
series. Scope any v1 to non-recurring events.
