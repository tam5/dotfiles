# Collecting various cursor rules


* Maintain consistency in capitalization and punctuation throughout the repository

---

stuff i should include:
* project structure
* api layer
* naming conventions
* data fetching
* ts rules
* react query, tw, t3env, check blazity


taylor otwell, polish


NOTE: The content of .cursorrules files will be appended to the global "Rules for AI" settings in Cursor
NOTE: Focus on providing repo-level context and guidelines, not just general coding practices
NOTE: .cursorrules can include information about project structure, architectural decisions, and commonly used libraries or methods

- do not invent custom stuff
- simplicity
- predictablity
- easy to change
- well typed, configurable stuff
- logging
- maintain single source of truth for logical pivots, ensure logically exclusive states are handled that way
- avoid conditionals
- avoid nesting
- hide implementation details, but don't remove control
- database design
- performance
- error handling
- telemetry
- rest design, resourceful, use openapi, proper error handling, consistent request/response formats, named routes, predictability, filtering thing, status codes, error messages, validation
- rate limiting, backpressure
- retries
- caching
- nextjs is frontend, dont put anythign sensitive
- no business logic in code, only api
- ui/ux
- reliability -> make it easy to recover from errors or network blips, etc. enable offline sync where appropriate
- modularize middleware, one thing at a time
- core things like middleware should get more extensive comments
- zero states, loading states, error states, single states, overflow states
- animations
- fetching latest documentation
- write and run tests?
- treeshaking in mind
- ID
- conciseness, terseness
- no temp variables
- Optimize Web Vitals (LCP, CLS, FID).
- load top/down
- Private Components: For components used only within specific pages, you can create a _components folder within the relevant /app subdirectory.
  - Avoid unnecessary else statements; use if-return pattern instead.
  - Use guard clauses to handle preconditions and invalid states early.
   - Implement proper error logging and user-friendly error messages. but don't mask the original actual error from the developers
   - prefer dogfoodable approaches, don't lock in to nextjs or any other particular vendor/framework
   - when to use optimistic vs not
- Prefix event handlers with "handle" (handleClick, handleSubmit)
- Use TypeScript for all code
- follow the best practices/community trends of the given framework/thing
- Use 'nuqs' for URL search parameter state management.



project specific:
- naming conventions
- file conventions
- folder structure

---

# Clean Code Guidelines

## Constants Over Magic Numbers
- Replace hard-coded values with named constants
- Use descriptive constant names that explain the value's purpose
- Keep constants at the top of the file or in a dedicated constants file

## Meaningful Names
- Variables, functions, and classes should reveal their purpose
- Names should explain why something exists and how it's used
- Consistent prefixes and suffixes should be used to disambiguate. For example, `isEnabled` rather than `enabled`, `imageURL` rather than `image`, and `timeoutInMilliseconds` rather than `timeout`.
- Avoid abbreviations unless they're universally understood

## Smart Comments
- Don't comment on what the code does - make the code self-documenting
- Use comments to explain why something is done a certain way
- Document APIs, complex algorithms, and non-obvious side effects

## SOLID
- SOLID principles are guidelines but not gospel
- Above all else, optimize for making code _easy to change_, rather than code that doesn't have to change
- Always strive for simplicity. Avoid premature optimization.

## Encapsulation & Abstraction
- Hide implementation details, but do not remove control
- Expose clear, predictable interfaces
- Avoid nested conditionals and complex logic flows

## Code Quality Maintenance
- Refactor continuously
- Fix technical debt early
- Leave code cleaner than you found it
- Let the ugly guide you




# Chatbot specific stuff

## No Apologies
Never use apologies.

## No Understanding Feedback
Avoid giving feedback about understanding in comments or documentation.

## No Whitespace Suggestions
Don't suggest whitespace changes.

## No Summaries
Don't summarize changes made.

## No Inventions
Don't invent changes other than what's explicitly requested. But you may make suggestions.

# NextJS

## Project Structure
- Use the App Router directory structure
- Place components in `app` directory for route-specific components
- Place shared components in `components` directory
- Place utilities and helpers in `lib` directory
- Use lowercase with dashes for directories (e.g., `components/auth-wizard`)

## Components
- Use Server Components by default
- Mark client components explicitly with 'use client'
- Wrap client components in Suspense with fallback
- Use dynamic loading for non-critical components
- Implement proper error boundaries

## Performance
- Optimize images: Use WebP format, size data, lazy loading
- Minimize use of 'useEffect' and 'setState'
- Favor Server Components (RSC) where possible
- Use dynamic loading for non-critical components
- Implement proper caching strategies

## Data Fetching
- Use Server Components for data fetching when possible
- Implement proper error handling for data fetching
- Use appropriate caching strategies
- Handle loading and error states appropriately
- Use @tanstack/react-query to manage syncing server state with client state
- Define all data fetchers in a centralized api lib

## Routing
- Use the App Router conventions
- Implement proper loading and error states for routes
- Use dynamic routes appropriately
- Handle parallel routes when needed

## Forms and Validation
- Use Zod for form validation
- Implement proper server-side validation in the api layer
- Handle form errors appropriately
- Show loading states during form submission
- Always give proper user feedback

## State Management
- Minimize client-side state
- Use React Context sparingly
- Prefer server state when possible
- Implement proper loading states 
- A minimal set of auth/session related state may be made globally available

# React

## Hooks
- Follow the Rules of Hooks
- Use custom hooks for reusable logic
- Hooks are only for hooking in to React lifecycle, otherwise make a regular function
- Keep hooks focused and simple
- Use appropriate dependency arrays in useEffect
- Implement cleanup in useEffect when needed
- Avoid nested hooks

## State Management
- Use useState for local component state
- Implement useReducer for complex state logic
- Keep state as close to where it's used as possible
- Avoid prop drilling through proper state management
- Use state management libraries only when necessary

## Performance
- Implement proper memoization (useMemo, useCallback)
- Use React.memo for expensive components
- Avoid unnecessary re-renders
- Implement proper lazy loading
- Use proper key props in lists
- Profile and optimize render performance

## Accessibility
- Use semantic HTML elements
- Implement proper ARIA attributes
- Ensure keyboard navigation
- Test with screen readers
- Handle focus management
- Provide proper alt text for images

# Tailwind

## Component Styling
- Use utility classes over custom CSS
- Use proper responsive design utilities
- Implement dark mode properly
- Use proper state variants
- Keep component styles consistent

## Layout
- Use Flexbox and Grid utilities effectively
- Implement proper spacing system
- Implement proper responsive breakpoints
- Use proper padding and margin utilities
- Implement proper alignment utilities

## Colors
- Use semantic color naming
- Implement proper color contrast
- Use opacity utilities effectively
- Configure custom colors properly
- Use proper gradient utilities
- Implement proper hover states

## Responsive Design
- Use mobile-first approach
- Implement proper breakpoints
- Use container queries effectively
- Handle different screen sizes properly
- Implement proper responsive typography
- Use proper responsive spacing

# Typescript

## Type System
- Prefer interfaces over types for object definitions
- Use type for unions, intersections, and mapped types
- Avoid using `any`, prefer `unknown` for unknown types
- Use strict TypeScript configuration
- Leverage TypeScript's built-in utility types
- Use generics for reusable type patterns

## Naming Conventions
- Use PascalCase for type names and interfaces
- Use camelCase for variables and functions
- Use UPPER_CASE for constants
- Use descriptive names with auxiliary verbs (e.g., isLoading, hasError)
- Suffix interfaces for React props with 'Props' (e.g., ButtonProps)

## Code Organization
- Keep type definitions close to where they're used
- Export types and interfaces from dedicated type files when shared
- Use barrel exports (index.ts) for organizing exports
- Place shared types in a `types` directory
- Co-locate component props with their components

## Functions
- Do not use explicit return types when they are not needed

## Best Practices
- Enable strict mode in tsconfig.json
- Use readonly for immutable properties
- Leverage discriminated unions for type safety
- Use type guards for runtime type checking
- Implement proper null checking
- Avoid type assertions unless necessary
