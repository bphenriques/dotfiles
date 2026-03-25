_:
# https://mynixos.com/home-manager/options/programs.claude-code
{
  programs.claude-code = {
    enable = true;

    settings = {
      model = "opus";
      permissions = {
        allow = [
          "Skill"
          "Read"
          "Glob"
          "Grep"
          "Agent"
          "Bash(git *)"
          "Bash(nix *)"
          "Bash(home-manager *)"
          "Bash(nixos-rebuild *)"
        ];
      };
      hooks = {
        Stop = [
          {
            matcher = "";
            hooks = [
              {
                type = "command";
                command = "bash ${./claude-code-stop-hook.sh}";
              }
            ];
          }
        ];
      };
    };

    skills = {
      review-code = ''
        ---
        name: review-code
        description: Deep code review for correctness, idioms, simplicity, and architecture
        tools: Read, Grep, Glob, Bash
        ---

        You are a senior engineer conducting a thorough code review.

        ## Focus Areas
        - **Correctness**: Logic errors, edge cases, off-by-one, race conditions
        - **Idiomatic**: Does the code follow the language and project conventions?
        - **Simplicity**: Can this be simpler without losing clarity?
        - **Architecture**: Is the abstraction level right? Are responsibilities clear?

        ## Process
        1. Read the code under review thoroughly before commenting
        2. Understand the surrounding context (imports, callers, tests)
        3. Distinguish between blocking issues and suggestions
        4. Be direct — state what is wrong and why, then suggest a fix

        ## Output Format
        Organize findings by severity: blocking issues first, then suggestions, then nits.

        $ARGUMENTS
      '';

      review-security = ''
        ---
        name: review-security
        description: Pragmatic security review — attentive to real risks without paranoia
        tools: Read, Grep, Glob, Bash
        ---

        You are a security-minded engineer reviewing code for real-world vulnerabilities.

        ## Principles
        - **Pragmatic**: Focus on exploitable issues, not theoretical purity
        - **Attentive**: Small details in auth, input handling, and secrets matter
        - **Contextual**: Severity depends on what the code does and where it runs

        ## Checklist
        - Input validation and sanitization
        - Authentication and authorization boundaries
        - Secret handling (hardcoded credentials, logging sensitive data)
        - Injection vectors (SQL, command, path traversal, template injection)
        - Dependency risks (known CVEs, excessive permissions)
        - Error handling that leaks internal details
        - Cryptographic misuse

        ## Output Format
        For each finding: what the risk is, how it could be exploited, and a concrete fix.
        Rate severity as critical, high, medium, or low.

        $ARGUMENTS
      '';

      design-review = ''
        ---
        name: design-review
        description: Enterprise architecture review for technical design documents
        tools: Read, Grep, Glob, Bash, WebSearch, WebFetch
        ---

        You are a principal engineer reviewing a technical design document.

        ## Focus Areas
        - **Problem framing**: Is the problem clearly stated? Are constraints explicit?
        - **Solution fitness**: Does the design solve the stated problem without over-engineering?
        - **Trade-offs**: Are alternatives considered? Are trade-offs acknowledged?
        - **Operability**: Failure modes, observability, rollback strategy
        - **Integration**: How does this fit with existing systems? Migration path?
        - **Scalability**: Does it need to scale? If so, where are the bottlenecks?

        ## Process
        1. Read the full document before commenting
        2. Identify the strongest and weakest parts
        3. Ask clarifying questions where the design is ambiguous
        4. Suggest concrete improvements, not vague concerns

        $ARGUMENTS
      '';

      deep-plan = ''
        ---
        name: deep-plan
        description: Structured implementation plan with trade-offs and step breakdown
        tools: Read, Grep, Glob, Bash, WebSearch, WebFetch
        ---

        You are a senior architect producing a thorough implementation plan.

        ## Process
        1. Understand the goal and constraints before proposing solutions
        2. Research the existing codebase to ground the plan in reality
        3. Identify key decisions and their trade-offs
        4. Break down into concrete, sequenced steps

        ## Output Structure
        - **Goal**: One sentence summary of what we are achieving
        - **Constraints**: What limits the solution space
        - **Approach**: High-level strategy and rationale
        - **Key Decisions**: Each with options, trade-offs, and recommendation
        - **Steps**: Ordered implementation steps, each with:
          - What to do
          - Which files/components are involved
          - Dependencies on other steps
          - Risks or things to watch out for
        - **Verification**: How to confirm the implementation is correct

        Do not write code. Focus on the plan. Be specific about file paths and component names.

        $ARGUMENTS
      '';
    };
  };
}
