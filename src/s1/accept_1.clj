(ns s1.accept-1
  (:require
   [hyperfiddle.rcf :refer [tests]]
   [spy.core :as spy]))

;; provided functions
(defn fake-mission [id user-id]
  {:mission-id id
   :user-id user-id
   :state :proposal})
(def fake-flag true)
(def fake-can-accept? true)
(def fake-has-contract? true)

(defn find-by-id [id user-id] (fake-mission id user-id))

(defn enabled-flag? [] fake-flag)

(defn can-accept? [user-id]
  fake-can-accept?)

(defn has-contract? [user-id]
  fake-has-contract?)

;; our fn
(defn accept [mission-id user-id]
  (let [mission (find-by-id mission-id user-id)]
    (if (nil? mission)
      {:error "Mission does not exist" :mission-id mission-id}
      (let [user-has-contract? (has-contract? user-id)
            flag-set? (enabled-flag?)
            self-accept? (can-accept? user-id)]
        (cond
          (and flag-set? user-has-contract? self-accept?) {:state :accepted :mission-id mission-id :user-id user-id}
          (and flag-set? user-has-contract? (not self-accept?)) {:state :pending_manager_approval :mission-id mission-id :user-id user-id}
          (and (not flag-set?) user-has-contract?) {:state :accepted :mission-id mission-id :user-id user-id}
          (and flag-set? (not user-has-contract?)) {:state :pending_contract_creation :mission-id mission-id :user-id user-id}
          :else {:error "Cannot process mission due to configuration"})))))

(tests
 (def mission-id 1)
 (def user-id 2)

 "Mission does not exist"
 (with-redefs
  [find-by-id (spy/spy (fn [_ _] nil))]
   (accept mission-id user-id) := {:error "Mission does not exist" :mission-id mission-id}
   (spy/called-once-with? find-by-id mission-id user-id) := true
   (spy/call-count enabled-flag?) := 0
   (spy/call-count has-contract?) := 0
   (spy/call-count can-accept?) := 0)

 "Mission exists, flag NOT enabled => accepted"
 (with-redefs
  [find-by-id (spy/spy (fn [mission-id user-id] {:mission-id mission-id :user-id user-id :state :pending}))
   enabled-flag? (spy/spy (fn [] false))]
   (accept mission-id user-id) := {:mission-id mission-id :user-id user-id :state :accepted}
   (spy/called-once-with? find-by-id mission-id user-id) := true
   (spy/called-once-with? enabled-flag?) := true
   (spy/call-count has-contract?) := 0
   (spy/call-count can-accept?) := 0)

 "Mission exists, flag enabled, has contract, can accept"
 (with-redefs
  [find-by-id (spy/spy (fn [mission-id user-id] {:mission-id mission-id :user-id user-id :state :pending}))
   enabled-flag? (spy/spy (fn [] true))
   has-contract? (spy/spy (fn [_] true))
   can-accept? (spy/spy (fn [_] true))]
   (accept mission-id user-id) := {:mission-id mission-id :user-id user-id :state :accepted}
   (spy/called-once-with? find-by-id mission-id user-id) := true
   (spy/called-once-with? enabled-flag?) := true
   (spy/called-once-with? has-contract? user-id) := true
   (spy/called-once-with? can-accept? user-id) := true)

 "Mission exists, flag enabled, doesn;t have contract, _"
 (with-redefs
  [find-by-id (spy/spy (fn [mission-id user-id] {:mission-id mission-id :user-id user-id :state :pending}))
   enabled-flag? (spy/spy (fn [] true))
   has-contract? (spy/spy (fn [_] false))]
   (accept mission-id user-id) := {:mission-id mission-id :user-id user-id :state :pending_contract_creation}
   (spy/called-once-with? find-by-id mission-id user-id) := true
   (spy/called-once-with? enabled-flag?) := true
   (spy/called-once-with? has-contract? user-id) := true
   (spy/call-count can-accept?) := 0)

 "Mission exists, flag enabled, has contract, CANNOT accept"
 (with-redefs
  [find-by-id (spy/spy (fn [mission-id user-id] {:mission-id mission-id :user-id user-id :state :pending}))
   enabled-flag? (spy/spy (fn [] true))
   has-contract? (spy/spy (fn [_] true))
   can-accept? (spy/spy (fn [_] false))]
   (accept mission-id user-id) := {:mission-id mission-id :user-id user-id :state :pending_manager_approval}
   (spy/called-once-with? find-by-id mission-id user-id) := true
   (spy/called-once-with? enabled-flag?) := true
   (spy/called-once-with? has-contract? user-id) := true
   (spy/called-once-with? can-accept? user-id) := true)

 nil)

