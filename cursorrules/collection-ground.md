# Collecting various cursor rules


* Maintain consistency in capitalization and punctuation throughout the repository


## Python

- uv
- don't install stuff
- use rich
- use env vars

- Place tests in `tests/` directory parallel to `src/`
- Keep configuration in `config/` or as environment variables

## Code Style
- Follow Black code formatting
- Use isort for import sorting
- Follow PEP 8 naming conventions:
  - snake_case for functions and variables
  - PascalCase for classes
  - UPPER_CASE for constants
- Maximum line length of 88 characters (Black default)
- Use absolute imports over relative imports

## Type Hints (needs review)
- Use type hints for all function parameters and returns
- Import types from `typing` module
- Use `Optional[Type]` instead of `Type | None`
- Use `TypeVar` for generic types
- Define custom types in `types.py`
- Use `Protocol` for duck typing

- Monitor application performance
- Implement proper logging

---
---


NOTE: The content of .cursorrules files will be appended to the global "Rules for AI" settings in Cursor
NOTE: Focus on providing repo-level context and guidelines, not just general coding practices
NOTE: .cursorrules can include information about project structure, architectural decisions, and commonly used libraries or methods

Add later:
# UI/UX Design
# Database Design
# Logging
# API Design

- community, framework, and trends above current implementation
- database design
- logging
- performance
- error handling
- telemetry
- rest design, resourceful, use openapi, proper error handling, consistent request/response formats, named routes, predictability, filtering thing, status codes, error messages, validation
- rate limiting, backpressure
- retries
- caching
- nextjs is frontend, dont put anythign sensitive
- ui/ux
- reliability -> make it easy to recover from errors or network blips, etc. enable offline sync where appropriate
- core things like middleware should get more extensive comments
- zero states, loading states, error states, single states, overflow states
- animations
- write and run tests?
- load top/down
- Private Components: For components used only within specific pages, you can create a _components folder within the relevant /app subdirectory.
- Implement proper error logging and user-friendly error messages. but don't mask the original actual error from the developers
- prefer dogfoodable approaches, don't lock in to nextjs or any other particular vendor/framework
- when to use optimistic vs not

