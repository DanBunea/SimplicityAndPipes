# Simplicity in practice

when your function is quite complicated should you divide it into several smaller functions and use a pipe? (threading macro in clojure)?

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
```

## Solutions

### [Solution 1](src/s1/accept_1.clj)



we make a [single function](src/s1/accept_1.clj#L26-L38) and test everything together resulting in quite [complex tests](src/s1/accept_1.clj#L40-L98) (especially compared to the ones in solution 2)

### [Solution 2](src/s2/accept_2.clj)

we break the [main function](src/s2/accept_2.clj#L64-L71) into several functions and use a pipe, then test the pipe and the individual functions separately

## Conclusion

Compare the readability of the two functions:
[solution 1](src/s1/accept_1.clj#L26-L38) vs 
[solution 2](src/s2/accept_2.clj#L64-L71)

Then imaging you'd have to modify or extend the logic later. Which one would be easier to work with? 