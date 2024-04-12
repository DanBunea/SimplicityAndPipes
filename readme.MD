# Simplicity in practice

## Problem: Mission Acceptance

## Background

we need to develop a function in Clojure that manages the acceptance of missions, using a mission id and a user id, based on various conditions fetched from a database. The mission is in a database, we have a flag in the database, we also have employment contracts and user permissions.

## Conditions to Check
1. **Mission Existence**: If no mission is found with the provided ID, the function should return {:error "Mission does not exist", :mission-id 999 ...}.
2. **Enabled Flag**: If the enabled flag is not set, the mission is automatically accepted {:state :accepted ...}.
3. **Employment Contract**: If the mission receiver (user-id) does not have an employment contract, the state should be set to `:pending_contract_creation`.
4. **User Permissions**: Depending on user permissions in the database, the mission might require co-validation by the manager. If the permissions allow for self-acceptance without a manager's approval and all other conditions are met, the mission should be marked as `:accepted`. If not, it should be marked as `:pending_manager_approval`.

## Example Usage
```clojure
(accept 123 12)  ; Might return {:state :accepted :mission-id 123 :user-id 12}
(accept 999 12)  ; Might return {:error "Mission does not exist", :mission-id 999 :user-id 12}
