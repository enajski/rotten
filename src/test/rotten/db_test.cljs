(ns rotten.db-test
  (:require [rotten.db :as db]
            [cljs.test :as t :refer [deftest is testing]]))

(deftest movement-test
  (with-redefs [db/state (atom {:world    {[0, 0] {:tile/kind     :tile.kind/floor
                                                   :tile/entities #{0}}
                                           [0, 1] {:tile/kind :tile.kind/floor}
                                           [0, 2] {:tile/kind :tile.kind/floor}}
                                :entities {0 {:entity/kind     :entity.kind/character
                                              :entity/position [0, 0]}
                                           1 {:entity/kind :entity.kind/character}}})]

    (testing "Move entity from tile to tile"
      (let [_!        (db/move-entity [0 0] [0 1] 0)
            new-state @db/state]
        (is (= #{}
               (get-in new-state [:world [0 0] :tile/entities])))
        (is (= #{0}
               (get-in new-state [:world [0 1] :tile/entities])))
        (is (= [0 1]
               (get-in new-state [:entities 0 :entity/position])))))

    (testing "Place new entity on tile"
      (let [entity-id 2
            entity    {:entity/kind :entity.kind/character}
            _!        (db/create-entity entity-id entity)
            _!        (db/place-entity [0 2] 1)
            new-state @db/state]
        (is (= entity
               (get-in new-state [:entities entity-id])))
        (is (= #{1}
               (get-in new-state [:world [0 2] :tile/entities])))
        (is (= [0 2]
               (get-in new-state [:entities 1 :entity/position])))))))

