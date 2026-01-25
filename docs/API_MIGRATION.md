# Snap to Servant API Migration

This document captures the differences between the Snap and Servant API implementations
to guide client migration and ensure API compatibility.

## Summary

The migration from Snap to Servant involves several categories of changes:
1. **Missing endpoints** - Functionality present in Snap but not yet in Servant
2. **Path/route changes** - Different URL structures
3. **HTTP method changes** - Different HTTP verbs for the same operation
4. **Response format changes** - Different JSON structures in responses
5. **Authentication changes** - Session-based to JWT-based

---

## 1. Missing Endpoints

### 1.1 `listPayableRequests`

**Snap Implementation:** `server/Aftok/Snaplet/Payments.hs:48`

```haskell
listPayableRequestsHandler :: S.Handler App App [(PaymentRequestId, SomePaymentRequestDetail)]
listPayableRequestsHandler = do
  uid <- requireUserId
  sid <- requireId "subscriptionId" SubscriptionId
  snapEval $ findPayableRequests uid sid
```

**Purpose:** Lists outstanding payment requests for a given subscription. Used when a
subscriber wants to see what payments are due.

**Snap Route:** `GET /?action=listPayableRequests&subscriptionId=<uuid>`

**Status:** NOT IMPLEMENTED in Servant

**Required Servant Route:** `GET /api/subscriptions/:subscriptionId/paymentRequests`

---

### 1.2 `projectWorkIndex`

**Snap Implementation:** `server/Aftok/Snaplet/WorkLog.hs:95`

```haskell
projectWorkIndex :: S.Handler App App (WorkIndex KeyedLogEntry)
projectWorkIndex = do
  uid <- requireUserId
  pid <- requireProjectId
  snapEval $ readWorkIndex pid uid
```

**Purpose:** Returns the complete work index for a project (all contributors' work
intervals). This is different from `userWorkIndex` which only returns the authenticated
user's contributions.

**Snap Route:** `GET /?action=projectWorkIndex&projectId=<uuid>`

**Status:** NOT IMPLEMENTED in Servant

**Required Servant Route:** `GET /api/projects/:projectId/workIndex`

---

## 2. Path/Route Differences

### 2.1 Username Validation

| Aspect | Snap | Servant |
|--------|------|---------|
| Path | `/check_username` | `/api/validate_username` |
| Method | GET | GET |
| Query Param | `username` | `username` |

**Recommendation:** Change Servant to use `/api/check_username` for compatibility.

---

### 2.2 Z-Address Validation

| Aspect | Snap | Servant |
|--------|------|---------|
| Path | `/check_zaddr` | `/api/validate_zaddr` |
| Method | GET | GET |
| Query Param | `zaddr` | `zaddr` |

**Recommendation:** Change Servant to use `/api/check_zaddr` for compatibility.

---

### 2.3 General Route Pattern

Snap uses query parameters (`?action=X&param=Y`) while Servant uses RESTful path
hierarchies (`/resource/:id/action`). This is an intentional architectural improvement
but may require client updates.

| Snap Pattern | Servant Pattern |
|--------------|-----------------|
| `/?action=create&projectId=X` | `/api/projects` (POST) |
| `/?action=get&projectId=X` | `/api/projects/:projectId` (GET) |
| `/?action=bid&auctionId=X` | `/api/auctions/:auctionId/bid` (POST) |

**Note:** This is a beneficial change for API clarity. Clients should be updated to
use the new RESTful patterns.

---

## 3. HTTP Method Differences

### 3.1 Event Amendment

| Aspect | Snap | Servant |
|--------|------|---------|
| Path | `/?action=amend&eventId=X` | `/api/events/:eventId/amend` |
| Method | **POST** | **PUT** |

**Recommendation:** Change Servant to use POST to match Snap, OR document this as an
intentional improvement (PUT is more semantically correct for amendments).

---

## 4. Response Format Differences

### 4.1 Username Check

**Snap Response:**
- Available: Empty 200 OK
- Taken: 400 Bad Request with error message

**Servant Response:**
```json
{
  "schemaVersion": "1.0",
  "UsernameCheckResponse": {
    "available": true
  }
}
```

**Recommendation:** Update Servant to match Snap behavior (empty 200 for available,
400 for taken) OR document this as an intentional improvement.

---

### 4.2 Z-Address Check

**Snap Response:**
- Valid: Returns `Zcash.Address` as JSON string
- Invalid: 400 Bad Request with error message

**Servant Response:**
```json
{
  "schemaVersion": "1.0",
  "ZAddrCheckResponse": {
    "valid": true,
    "address": "<address>"
  }
}
```

**Recommendation:** Update Servant to match Snap behavior OR document as improvement.

---

### 4.3 Accept Invitation

**Snap Response:** Empty 200 OK

**Servant Response:** 204 No Content

**Recommendation:** Both are valid REST responses for success with no body. 204 is
more semantically correct. Document as intentional.

---

### 4.4 BIP70 Payment Request

**Snap Response:**
```haskell
(PaymentRequestId, Bitcoin.PaymentRequest)
```
Returns both the request ID and the full payment request structure.

**Servant Response:**
Raw protobuf bytes only (for BIP70 compatibility).

**Note:** The Servant approach is correct for BIP70 protocol compliance. The request
ID is available from the URL path parameter.

---

## 5. Authentication Differences

| Aspect | Snap | Servant |
|--------|------|---------|
| Mechanism | Session-based (cookies) | JWT-based (Bearer token) |
| Library | `Snap.Snaplet.Auth` | `servant-auth-server` |
| User Extraction | `requireUserId` (session) | `AuthResult AuthenticatedUser` (JWT) |

**Impact:** Clients must switch from cookie-based authentication to JWT Bearer tokens.
This is a significant change that affects all authenticated endpoints.

---

## 6. WorkLog Differences

### 6.1 Log Work Endpoint

**Snap:** Single `logWorkHandler` that takes an event constructor parameter to
determine start vs stop.

**Servant:** Separate `/logStart` and `/logEnd` endpoints.

| Snap | Servant |
|------|---------|
| `POST /?action=log` with event type in body | `POST /api/user/projects/:pid/logStart` |
| `POST /?action=log` with event type in body | `POST /api/user/projects/:pid/logEnd` |

**Recommendation:** This is an improvement for API clarity. Document the new endpoints.

---

### 6.2 User Work Index vs Project Work Index

**Snap has two distinct endpoints:**
1. `projectWorkIndex` - All contributors' work for a project
2. `userWorkIndex` - Only the authenticated user's work

**Servant currently only has:**
- `userWorkIndexHandler` - Only the authenticated user's work

**Status:** `projectWorkIndex` is MISSING from Servant.

---

## 7. Migration Checklist

### Completed (Server-Side)
- [x] Add `listPayableRequests` endpoint (`GET /api/subscriptions/:subscriptionId/paymentRequests`)
- [x] Add `projectWorkIndex` endpoint (`GET /api/projects/:projectId/workIndex`)
- [x] Rename `validate_username` to `check_username`
- [x] Rename `validate_zaddr` to `check_zaddr`

### Intentional Improvements (Client Updates Required)
The following changes are intentional improvements over the Snap API. Clients must be updated:

- **Username/ZAddr check responses**: Return structured JSON with explicit boolean fields
  instead of using HTTP status codes for business logic
- **Event amendment**: Uses `PUT` (semantically correct for modifications) instead of `POST`
- **Accept invitation**: Returns `204 No Content` instead of empty `200 OK`
- **RESTful paths**: Uses proper path hierarchies instead of query parameters
- **JWT authentication**: Uses Bearer tokens instead of session cookies
- **Separate logStart/logEnd**: Explicit endpoints instead of event type in body

---

## 8. Version Strategy

Consider implementing API versioning to support both old and new clients:

```
/api/v1/...  - Snap-compatible routes and responses
/api/v2/...  - New RESTful routes and responses (current Servant)
```

This allows gradual client migration without breaking existing integrations.
