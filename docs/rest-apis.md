# Generic REST API Specification

## 1. Introduction

This document outlines the design principles and conventions for the [Your Company/Project Name] REST API. The API provides programmatic access to resources, enabling developers to build integrations and applications.

Our API is designed around REST principles, utilizing predictable resource-oriented URLs, accepting and returning JSON-encoded bodies (`application/json`), and leveraging standard HTTP verbs, status codes, and authentication mechanisms. It draws inspiration from common REST best practices and specifications like JSON:API to ensure consistency and ease of use.

The core philosophy is to encapsulate business logic within the API, providing a stable, maintainable, and predictable interface, minimizing the need for complex client-side libraries. We prioritize developer experience and aim for unambiguous API behavior.

## 2. Core Principles

* **Resource-Oriented:** The API is structured around resources (e.g., contacts, notes).
* **Standard HTTP:** Uses standard HTTP methods (GET, POST, PATCH, DELETE) and status codes.
* **JSON:** Uses JSON (`application/json`) for request and response bodies.
* **Predictability:** Follows consistent patterns for URLs, naming, and response structures.
* **Statelessness:** Each request from a client must contain all information needed to understand and process the request. Server-side sessions are not used to store client state between requests.

## 3. URI Structure & Naming

### 3.1. Base URI

All API endpoints are relative to a base URI, which includes the API version.
Example: `https://api.yourdomain.com/v1/`

### 3.2. Resource URLs

URLs are predictable and resource-oriented. Standard actions are mapped to HTTP verbs:

| Verb        | URI                       | Action             | Description                 |
| :---------- | :------------------------ | :----------------- | :-------------------------- |
| `GET`       | `/contacts`               | `contacts.list`    | List all contacts           |
| `POST`      | `/contacts`               | `contacts.create`  | Create a new contact        |
| `GET`       | `/contacts/{contactId}`   | `contacts.show`    | Show a single contact       |
| `PATCH`     | `/contacts/{contactId}`   | `contacts.update`  | Update a single contact (partial) |
| `DELETE`    | `/contacts/{contactId}`   | `contacts.destroy` | Destroy a single contact    |
| `POST`      | `/contacts/search`        | `contacts.search`  | Advanced search for contacts (See Section 8.4.2) |

* **`{resourceId}`:** Represents the unique identifier for a specific resource instance.

### 3.3. Nested Resources

Relationships between resources are represented using nested URLs where appropriate (i.e., where a clear parent-child ownership exists).

Example: Notes belonging to a contact.

| Verb        | URI                                       | Action                   | Description                       |
| :---------- | :---------------------------------------- | :----------------------- | :-------------------------------- |
| `GET`       | `/contacts/{contactId}/notes`             | `contacts.notes.list`    | List all notes for a contact      |
| `POST`      | `/contacts/{contactId}/notes`             | `contacts.notes.create`  | Create a new note for a contact   |
| `GET`       | `/contacts/{contactId}/notes/{noteId}`    | `contacts.notes.show`    | Show a specific note for a contact|
| `PATCH`     | `/contacts/{contactId}/notes/{noteId}`    | `contacts.notes.update`  | Update a specific note for a contact|
| `DELETE`    | `/contacts/{contactId}/notes/{noteId}`    | `contacts.notes.destroy` | Destroy a specific note for a contact|

### 3.4. Shallow Nesting

For convenience and when the parent ID isn't necessary or available, "shallow" routes allow direct access to child resources via their unique ID.

Example: Accessing a note directly.

| Verb        | URI                 | Action          | Description              |
| :---------- | :------------------ | :-------------- | :----------------------- |
| `GET`       | `/notes/{noteId}`   | `notes.show`    | Show a single note       |
| `PATCH`     | `/notes/{noteId}`   | `notes.update`  | Update a single note     |
| `DELETE`    | `/notes/{noteId}`   | `notes.destroy` | Destroy a single note    |

### 3.5. Naming Conventions

* **Resource Names:** Use plural nouns (e.g., `/contacts`, `/orders`).
    * *Good:* `/users`, `/posts`
    * *Bad:* `/user`, `/getPosts`
* **Path Segments:** Use lowercase letters. Separate words with hyphens (`-`) (kebab-case).
    * *Good:* `/user-profiles`, `/order-items`
    * *Bad:* `/userProfiles`, `/order_items`
* **Avoid Verbs in URIs:** Use HTTP methods to indicate actions. The `/search` endpoint is a conventional exception for complex querying.
    * *Good:* `POST /contacts` (to create)
    * *Bad:* `POST /create-contact`
* **Nesting vs. Compound Words:**
    * Use nesting for clear parent-child *object relationships* where the parent has an ID (e.g., `/contacts/{contactId}/notes`).
    * Use hyphenated compound words for *conceptually related* resources that don't have a strict ownership model or where the "parent" isn't a specific instance (e.g., `/email-templates`, not `/emails/templates`).

## 4. Request Format

* **Content-Type:** Clients sending request bodies (POST, PATCH) MUST use `Content-Type: application/json`.
* **Accept Header:** Clients SHOULD specify the desired response format using the `Accept` header (e.g., `Accept: application/json`).
* **HTTP Methods:** Use standard HTTP methods as described in Section 3.2.
    * `GET`: Retrieve resources.
    * `POST`: Create new resources or perform actions (like search).
    * `PATCH`: Update an existing resource partially.
    * `DELETE`: Remove a resource.

## 5. Response Format

* **Content-Type:** Responses with a body WILL have `Content-Type: application/json`.
* **Top-Level Structure:** All responses are JSON objects containing one or more of the following top-level keys:
    * `data`: The primary resource object or array of resource objects.
    * `errors`: An array of error objects (present only if an error occurred).
    * `links`: An object containing links related to the response (e.g., `self`, pagination).
    * `meta`: An object containing non-standard meta-information (e.g., pagination counts).
* **Mutual Exclusivity:** The `data` and `errors` keys are mutually exclusive; only one will be present in a response.
* **Resource Objects:** Every resource object within the `data` key MUST contain:
    * `id`: A string containing the resource's unique identifier. IDs are always treated as opaque strings.
    * `type`: A string identifying the resource type (e.g., `"contact"`, `"note"`). This should be the singular form of the resource name.
* **Timestamps:** Standard timestamp fields `createdAt` and `updatedAt` should be included where applicable.

### 5.1. Single Resource Response

```json
{
  "data": {
    "id": "123",
    "type": "contact",
    "attributes": {
      "firstName": "John",
      "lastName": "Doe",
      "email": "john.doe@example.com",
      "createdAt": "2024-04-01T10:00:00Z",
      "updatedAt": "2024-04-01T12:30:00Z"
      // ... other attributes
    },
    "relationships": {
      // ... optional relationships
    },
    "links": {
      "self": "https://api.yourdomain.com/v1/contacts/123"
    }
  },
  "links": {
    "self": "https://api.yourdomain.com/v1/contacts/123"
  }
}
```

### 5.2. Collection Response

```json
{
  "data": [
    {
      "id": "123",
      "type": "contact",
      "attributes": {
        "firstName": "John",
        "lastName": "Doe",
        // ...
        "createdAt": "2024-04-01T10:00:00Z",
        "updatedAt": "2024-04-01T12:30:00Z"
      },
      "links": {
        "self": "https://api.yourdomain.com/v1/contacts/123"
      }
    },
    {
      "id": "456",
      "type": "contact",
      "attributes": {
        "firstName": "Jane",
        "lastName": "Smith",
        // ...
        "createdAt": "2024-03-15T09:00:00Z",
        "updatedAt": "2024-03-20T16:45:00Z"
      },
      "links": {
        "self": "https://api.yourdomain.com/v1/contacts/456"
      }
    }
  ],
  "links": {
    "self": "https://api.yourdomain.com/v1/contacts?page[number]=1&page[size]=10",
    "first": "https://api.yourdomain.com/v1/contacts?page[number]=1&page[size]=10",
    "prev": null,
    "next": "https://api.yourdomain.com/v1/contacts?page[number]=2&page[size]=10"
  },
  "meta": {
    "total": 55 // or "estimatedTotal" or omitted depending on resource/query
  }
}
```

### 5.3. Error Response

See Section 10: Error Handling.

## 6. HTTP Status Codes

The API uses standard HTTP status codes to indicate the success or failure of a request.

### 2xx Success:
* **200 OK:** Request successful (GET, PATCH). Response includes a body.
* **201 Created:** Resource successfully created (POST). Location header contains URL of the new resource. Response includes representation of the created resource.
* **204 No Content:** Request successful, but no response body needed (DELETE).

### 4xx Client Errors:
* **400 Bad Request:** Invalid request syntax, parameters, or payload structure. Also used if mutually exclusive filter parameters are combined (See Section 8.4.1), or if page[size] exceeds the maximum limit.
* **401 Unauthorized:** Authentication failed or credentials missing.
* **403 Forbidden:** Authenticated user lacks permission to perform the action.
* **404 Not Found:** Requested resource does not exist.
* **405 Method Not Allowed:** HTTP method used is not supported for this resource.
* **409 Conflict:** Request conflicts with the current state of the resource (e.g., duplicate creation).
* **413 Payload Too Large:** Could be used if a filters GET parameter exceeds server limits (alternative to generic 400).
* **414 URI Too Long:** Could be used if the GET request URI exceeds server limits (alternative to generic 400).
* **415 Unsupported Media Type:** Content-Type of request is not supported.
* **422 Unprocessable Entity:** Request was well-formed but contained semantic errors (e.g., validation failures). Response body contains error details.
* **429 Too Many Requests:** Rate limit exceeded.

### 5xx Server Errors:
* **500 Internal Server Error:** Generic server error. Avoid using if possible; provide more specific errors.
* **503 Service Unavailable:** Server is temporarily unable to handle the request (e.g., maintenance).

## 7. Authentication

Authentication is required for most endpoints. The API uses Bearer Tokens passed in the Authorization header.

```
Authorization: Bearer <your_api_token>
```

Specific details on obtaining and managing API tokens are documented separately. Unauthenticated requests will receive a 401 Unauthorized response.

## 8. Common Operations

### 8.1. Pagination

Endpoints returning collections are paginated.

**Parameters:** Passed via query string, nested under the page key.
* **page[size]=N:** Specifies the number of results per page.
  * Default: 25
  * Maximum: 100 (Requests with page[size] > 100 will receive a 400 Bad Request error).
* **page[number]=N:** Used for page-based pagination (requests page N).
* **page[cursor]=X:** Used for cursor-based pagination (requests results after cursor X). (Note: Specific pagination method depends on the resource).

**Response:**
* **links:** Contains pagination links (first, last, prev, next) as appropriate. next will be null if on the last page, prev will be null if on the first page.
* **meta:** May contain pagination counts (see Section 8.2).

**Example (Page-based):** GET /contacts?page[number]=2&page[size]=25
**Example (Cursor-based):** GET /activities?page[size]=50&page[cursor]=aBcDeFg123

### 8.2. Including Totals

Some collection endpoints may support including total counts in the meta object.

**Availability:** Varies by resource. May be included by default, optional, estimated, or unavailable.

**Requesting Optional Totals:** Use query parameters:
* **includeTotalCount=true:** Request an exact total count (if supported and performant).
* **includeEstimatedTotalCount=true:** Request an estimated total (if supported).

**Response:** If included, the count appears in the meta object:
```json
meta: { "total": 1234 }
```
```json
meta: { "estimatedTotal": 1200 }
```

### 8.3. Sorting

Collections can be sorted using the sort query parameter.

**Format:** sort=field1,-field2,...
* Comma-separated list of fields to sort by.
* Order matters; sorting is applied sequentially.
* Default direction is ascending (ASC).
* Prefix a field with a hyphen (-) for descending (DESC) order.
* Dot notation can be used to sort by related resource fields (e.g., sort=contact.lastName).

**Example:** GET /notes?sort=-createdAt,title (Sort by creation date descending, then title ascending).

**Payload Alternative (for POST /search):** Sorting can be specified in the request body:
```json
{
  "sort": [
    { "field": "createdAt", "direction": "desc" },
    { "field": "title", "direction": "asc" }
  ]
  // ... other payload keys like filters ...
}
```

### 8.4. Filtering

Filtering allows clients to request a subset of resources based on specific criteria. Filtering is primarily performed via GET requests, offering two mutually exclusive methods.

#### 8.4.1. Filtering via GET Requests

GET requests support filtering using one of the following methods per request:

**Method 1: Basic Field Filters**
* **Format:** filter[fieldName]=value
* **Logic:** Multiple filter parameters act as a logical AND.
* **Operators:**
  * Default is equality (=).
  * Comma-separated values for a single field act as OR (equivalent to IN): filter[status]=active,pending.
  * Wildcard (*) may be supported for string fields (check resource docs): filter[email]=*@example.com.
* **Limitations:** Only supports basic equality/IN checks and implicit AND. Complex logic requires the Encoded JSON Filter method.
* **Example:** GET /contacts?filter[status]=active&filter[city]=New York

**Method 2: Encoded JSON Filter**
* **Format:** filters=<URL-encoded JSON string>
* **Logic:** Allows complex filtering logic, including nested AND/OR conditions and various operators, by encoding a JSON structure into the query string. The JSON structure used is the same as the filters array expected by the POST /resource/search endpoint (see Section 8.4.2).
* **Structure:** The JSON object to be encoded should contain a single key filters whose value is an array of filter objects (field filters or logical filters) as defined in Section 8.4.2.

**Example JSON (before encoding):**
```json
{
  "filters": [
    {
      "operator": "or",
      "filters": [
        { "field": "hiredAt", "operator": ">=", "value": "2001-01-01T00:00:00Z" },
        { "field": "name", "operator": "in", "value": ["Ed Truck", "Tod Packer"] }
      ]
    }
  ]
}
```

**Encoding Example (JavaScript):**
```javascript
const filterObject = {
  filters: [
    {
      operator: "or",
      filters: [
        { field: "hiredAt", operator: ">=", value: "2001-01-01T00:00:00Z" },
        { field: "name", operator: "in", value: ["Ed Truck", "Tod Packer"] }
      ]
    }
  ]
};

// 1. Convert the JavaScript object to a JSON string
const jsonString = JSON.stringify(filterObject);
// 2. URL-encode the JSON string
const filtersParam = encodeURIComponent(jsonString);

// Example usage:
const url = `/contacts?filters=${filtersParam}`;
// url would be "/contacts?filters=%7B%22filters%22%3A%5B%7B%22operator%22%3A%22or%22%2C%22filters%22%3A%5B%7B%22field%22%3A%22hiredAt%22%2C%22operator%22%3A%22%3E%3D%22%2C%22value%22%3A%222001-01-01T00%3A00%3A00Z%22%7D%2C%7B%22field%22%3A%22name%22%2C%22operator%22%3A%22in%22%2C%22value%22%3A%5B%22Ed%20Truck%22%2C%22Tod%20Packer%22%5D%7D%5D%7D%5D%7D"
```

**Example GET Request:** GET /contacts?filters=%7B%22filters%22%3A%5B...%5D%7D (using the encoded string from above)

**Mutual Exclusivity:**
* A request MUST NOT include both basic filter[fieldName] parameters and the filters parameter (containing the encoded JSON).
* If both are provided, the API MUST respond with a 400 Bad Request error.

**URL Length Considerations:**
* Using the filters parameter with complex JSON can lead to long URLs. Browsers and servers have varying URL length limits (often around 2000-8000 characters).
* Exceeding these limits will result in errors (potentially 414 URI Too Long or 413 Payload Too Large, depending on server configuration, or a generic 400 Bad Request).
* If your query consistently generates URLs approaching these limits, it might indicate excessive complexity for a synchronous GET request. Consider if the query can be simplified, broken down, or if an alternative pattern like asynchronous processing or the POST /resource/search endpoint (Section 8.4.2) is more appropriate for the use case.

#### 8.4.2. Advanced Filtering via POST Request (POST /resource/search)

For scenarios where GET request URL length limits are a concern, or when non-cacheability is acceptable, a dedicated POST /resource/search endpoint is available. This endpoint accepts the complex filter structure directly in the JSON request body.

**Request Body:** Accepts a JSON payload containing a filters key. May also include sort and page keys.

**Structure:** filters is an array containing filter objects. Filter objects can be:
* **Field Filters:** Apply a condition to a specific field.
  * field: The name of the field (can use dot notation for relationships, e.g., contact.name).
  * operator: The comparison operator (e.g., =, !=, >, <, >=, <=, in, not in, like, not like, is_null, is_not_null).
  * value: The value to compare against (type depends on field and operator; array for in/not in).
* **Logical Filters:** Combine multiple child filters.
  * operator: and or or.
  * filters: An array of nested filter objects (field or logical).

**Use Cases:** Suitable for extremely complex queries or when embedding the filter logic in the request body is preferred over URL parameters. Remember that POST requests are generally not cached by HTTP intermediaries.

**Example:**
```
POST /contacts/search
Content-Type: application/json

{
  "filters": [
    {
      "operator": "or",
      "filters": [
        { "field": "hiredAt", "operator": ">=", "value": "2001-01-01T00:00:00Z" },
        { "field": "name", "operator": "in", "value": ["Ed Truck", "Tod Packer"] }
      ]
    }
  ],
  "page": { "size": 20 },
  "sort": [{ "field": "name", "direction": "asc" }]
}
```

### 8.5. Including Relationships

Related resources can be embedded in the response using the include query parameter.

**Format:** include=relationship1,relationship2,...
* **Dot Notation:** Include nested relationships: include=contact,contact.company. This embeds the contact related to the primary resource, and also the company related to that contact.
* **Availability:** Depends on the resource and defined relationships. Check resource documentation.

**Example:** GET /notes/789?include=contact,author

### 8.6. Sparse Fieldsets

Clients can request only specific fields for a resource type using the fields query parameter.

**Format:** fields[resourceType]=field1,field2,...
* **Benefit:** Reduces response size.
* **Requirement:** The id and type fields are always included, even if not explicitly requested.

**Example:** GET /contacts?include=company&fields[contacts]=firstName,email&fields[companies]=name (Return only firstName and email for contacts, and only name for included companies).

## 9. Field and Value Conventions

* **Field Names:** Use camelCase (e.g., firstName, orderId).
* **Suffixes:** Use standard suffixes where appropriate to clarify meaning or type:
  * At for timestamps (e.g., createdAt, approvedAt).
  * Id for foreign keys (e.g., contactId).
  * Url for fields containing URLs (e.g., avatarUrl instead of just avatar).
  * Count for fields representing a count (e.g., loginCount).
* **Units:** Include units in names for ambiguous quantities (e.g., durationMs, timeoutSeconds).
* **Case Sensitivity:** All resource names, field names, and query parameter values are case-sensitive unless otherwise specified.
* **Timestamps:** All timestamps are returned in ISO 8601 format with UTC timezone indicator (YYYY-MM-DDTHH:MM:SSZ). Example: 2024-04-01T14:30:00Z. Timestamps sent in requests should also follow this format.
* **Null Values:** null is used to represent missing or inapplicable values. It is distinct from an empty string ("") or empty array ([]). In PATCH requests, providing a field with a null value explicitly sets that field to null, whereas omitting the field leaves its value unchanged.
* **IDs:** Resource id fields are always strings and should be treated as opaque by clients. Do not assume they are numeric or sequential.

## 10. Error Handling

When an error occurs, the API responds with an appropriate 4xx or 5xx status code and a JSON body containing an errors array.

**Structure:** Each object in the errors array follows JSON:API-inspired recommendations:
* **id:** A unique identifier for this specific occurrence of the problem.
* **status:** The HTTP status code applicable to this problem (string).
* **code:** An application-specific error code (string).
* **title:** A short, human-readable summary of the problem.
* **detail:** A human-readable explanation specific to this occurrence of the problem.
* **source:** (Optional) An object indicating the source of the error:
  * pointer: A JSON Pointer [RFC6901] to the associated entity in the request document (e.g., /data/attributes/email).
  * parameter: A string indicating the name of a query parameter that caused the error (e.g., filter or filters).
* **meta:** (Optional) A meta object containing non-standard meta-information about the error.

**Example (Validation Error - 422):**
```
HTTP/1.1 422 Unprocessable Entity
Content-Type: application/json

{
  "errors": [
    {
      "id": "a1b2c3d4-e5f6-7890-1234-567890abcdef",
      "status": "422",
      "code": "VALIDATION_ERROR",
      "title": "Invalid Attribute",
      "detail": "Email is not a valid email address.",
      "source": { "pointer": "/data/attributes/email" }
    },
    {
      "id": "b2c3d4e5-f6a7-8901-2345-67890abcdef0",
      "status": "422",
      "code": "VALIDATION_ERROR",
      "title": "Missing Required Attribute",
      "detail": "First name cannot be blank.",
      "source": { "pointer": "/data/attributes/firstName" }
    }
  ]
}
```

**Example (Conflicting Filters - 400):**
```
HTTP/1.1 400 Bad Request
Content-Type: application/json

{
  "errors": [
    {
      "id": "c3d4e5f6-a7b8-9012-3456-7890abcdef01",
      "status": "400",
      "code": "INVALID_PARAMETERS",
      "title": "Mutually Exclusive Filters Used",
      "detail": "Cannot use both 'filter[field]' and 'filters' parameters in the same request.",
      "source": { "parameter": "filter, filters" }
    }
  ]
}
```

## 11. Versioning

The API is versioned to allow for backward-incompatible changes. The version is specified in the URI path.

* **Format:** /v1/..., /v2/...
* **Current Version:** v1

Clients SHOULD always include the version number in their requests.

## 12. Request IDs

To aid in debugging and traceability, every API response includes a unique request identifier in the X-Request-Id HTTP header.

* **Header:** X-Request-Id: <unique_identifier>

Clients SHOULD log this ID along with any issues encountered when reporting problems to aid support and troubleshooting.

## 13. Rate Limiting

API requests may be subject to rate limiting to ensure fair usage and stability.

* **Response Codes:** If the rate limit is exceeded, the API will respond with a 429 Too Many Requests status code.
* **Headers:** Responses MAY include the following headers to indicate the client's current rate limit status:
  * X-RateLimit-Limit: The total number of requests allowed in the current window.
  * X-RateLimit-Remaining: The number of requests remaining in the current window.
  * X-RateLimit-Reset: The time (UTC epoch seconds) when the window resets.
  * Retry-After: (Included with 429) The number of seconds to wait before making another request.

## 14. Idempotency

* **GET, HEAD, OPTIONS, DELETE** methods MUST be idempotent. Making multiple identical requests should have the same effect as a single request.
* **PATCH** requests are NOT required to be idempotent, although they can be implemented idempotently. Clients should be aware that retrying a PATCH request might result in unintended state if the first request succeeded but the response was lost.
* **POST** requests are generally NOT idempotent. However, mechanisms like passing an Idempotency-Key header might be supported for specific POST operations (like resource creation) to safely retry requests without creating duplicates. (Details would be specified per-endpoint if applicable).
