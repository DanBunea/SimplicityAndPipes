(ns s2.accept-2
  (:require
   [hyperfiddle.rcf :refer [tests]]
   [spy.core :as spy]))

(defn fake-mission [id user-id]
  {:mission-id id
   :user-id user-id
   :state :proposal})
(def fake-flag true)
(def fake-can-accept true)
(def fake-has-contract true)

(defn find-by-id [{:keys [mission-id user-id] :as ctx}]
  (let [mission (fake-mission mission-id user-id)]
    (if  mission
      (merge ctx mission)
      (throw (ex-info "Mission does not exist" (assoc ctx :error "Mission does not exist"))))))

(defn assoc-if [ctx prop val condition]
  (if condition
    (assoc ctx prop val)
    ctx))

(defn process-flag [ctx] (assoc-if ctx :state :accepted fake-flag))
(defn check-contract [ctx] (assoc-if ctx :state :pending_contract_creation (not fake-has-contract)))
(defn check-can-accept [ctx]
  (assoc ctx :state (if fake-can-accept :accepted :pending_manager_approval)))

(tests
 (def mission-id 3)
 (def user-id 4)
 "inidividual pipe fns"

 "fake-mission - return mission when exists"
 (with-redefs [fake-mission (spy/spy (fn [_ _] {:state :proposal}))]
   (find-by-id {:mission-id mission-id :user-id user-id}) := {:mission-id mission-id :user-id user-id :state :proposal}
   (spy/called-once-with? fake-mission mission-id user-id) := true)

 "fake-mission -throws when no mission when doesn't exist"
 (with-redefs [fake-mission (spy/spy (fn [_ _] nil))]
   (find-by-id {:mission-id mission-id :user-id user-id}) :throws clojure.lang.ExceptionInfo
   (spy/called-once-with? fake-mission mission-id user-id) := true)

 (def ctx {:mission-id mission-id :user-id user-id :state :proposal})

 (with-redefs [fake-flag true]
   (process-flag ctx) := (assoc ctx :state :accepted))
 (with-redefs [fake-flag false]
   (process-flag ctx) := ctx)

 (with-redefs [fake-has-contract true]
   (check-contract ctx) := ctx)
 (with-redefs [fake-has-contract false]
   (check-contract ctx) := (assoc ctx :state :pending_contract_creation))

 (with-redefs [fake-can-accept true]
   (check-can-accept ctx) := (assoc ctx :state :accepted))
 (with-redefs [fake-can-accept false]
   (check-can-accept ctx) := (assoc ctx :state :pending_manager_approval))

 nil)

(defn accept [mission-id user-id]
  (try
    (-> {:mission-id mission-id :user-id user-id}
        find-by-id
        process-flag
        check-contract
        check-can-accept)
    (catch Exception e (ex-data e))))

(tests
 (def mission-id 5)
 (def user-id 6)

 (defn- pipe [step]
   (fn [a & _]
     (if (:pipe a)
       (update a :pipe conj step)
       (assoc a :pipe [step]))))

 "Passes the entire pipe in the right order"
 (with-redefs
  [find-by-id (pipe :find-by-id)
   process-flag (pipe :process-flag)
   check-contract (pipe :check-contract)
   check-can-accept (pipe :check-can-accept)]
   (accept mission-id user-id) := {:mission-id mission-id :user-id user-id
                                   :pipe [:find-by-id
                                          :process-flag
                                          :check-contract
                                          :check-can-accept]})

 "Returns an error when find-by-id throws"
 (with-redefs [find-by-id (spy/spy (fn [ctx] (throw (ex-info "Mission does not exist" (assoc ctx :error "Mission does not exist")))))]
   (accept mission-id user-id) := {:mission-id mission-id :user-id user-id :error "Mission does not exist"}))