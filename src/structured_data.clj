(ns structured-data)

(defn do-a-thing [x]
   (let [x2 (+ x x)]
      (Math/pow x2 x2))
)

(defn spiff [v]
 (+ (get v 0) (get v 2))
)

(defn cutify [v]
  (conj v "<3")
)

(defn spiff-destructuring [v]
  (let [[x y] (get v 0) (get v 2)]
   (+ x y)
  )
)

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
   sign( (- x2 x1))
  )
)

(def sign [x]
 (if (< x 0) (- x) x)
)

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
   sign( (- y2 y1) )
  )
)

(defn square? [rectangle]
  (if (== (height rectangle) (width rectangle)) true false)
)

(defn area [rectangle]
  (* (height rectangle) (width rectangle) )
)

(defn contains-point? [rectangle point]
  (let [[[h w] [x y]] [[(width rectangle) (height rectangle)] [point]]]
   (if (and (<= x h) (<= y w)) true false))
)



(defn contains-rectangle? [outer inner]
  (if (> (area outer) (area inner)) true false)
)


(defn title-length [book]
  (count (:title book)
)

(defn author-count [book]
  (count (:authors book)
)

(defn multiple-authors? [book]
  (if (> (author-count book) 1) true false
)

(defn add-author [book new-author]
  (let [[authrs] [(:authors book)]]
   (conj authrs new-author)
  )
)

(defn alive? [author]
  (if (contains? author death-year) false true)
)

(defn element-lengths [collection]
  (map cound collection)
)

(defn second-elements [collection]
  (let [snd (fn [x] (get x 1))]
   (map snd collection)
  )
)
)

(defn titles [books]
  (map :title books)
)

(defn monotonic? [a-seq]
  (if (or (apply <= a-seq) (apply >= a-seq)) true false)
)

(defn stars [n]
  (apply str (repeat n \*))
)

(defn toggle [a-set elem]
  (if (contains? a-set elem)
      (disj a-set elem)
      (conj a-set elem)
  )
)

(defn contains-duplicates? [a-seq]
  if (== (count a-seq) (count (set a-seq))) false true
)

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book)))
)

(defn has-author? [book author]
  (contains? (:authors book) author)
)

(defn authors [books]
 (let [author-names (fn [book] (:authors book))]
  (set (apply clojure.set/union (map author-names books))))
)


(defn all-author-names [books]
  (set (map :name (authors books)))
)


(defn author->string [author]
   (let [[name, birth, death] [(:name author) (str (:birth-year author)) (str (:death-year author))]]
       (if (not (alive? author)) (str name " (" birth " - " death ")")
       (if (empty? (str birth)) name  (str name " (" birth " - )") ) )
   )
)


(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
)

(defn book->string [book]
  (let [[title, authors] [(:title book), (authors->string (:authors book))]]
       (str title ", written by " authors)
  )
)

(defn books->string [books]
  (let [[nbooks] [(count books)]]
       (let [[st] [(apply str (interpose ". " (map book->string books)))]]
            (cond
                (== nbooks 1) (str "1 book. " st ".")
                (>  nbooks 1)  (str nbooks " books. " st ".")
                :else          "No books."
            )
        )
    )
)

(defn books-by-author [author books]
  (filter (fn [book] (has->author? book author)) books)
)

(defn author-by-name [name authors]
  (let [[result] [(filter (fn [author] (= (:name author) name)) authors)]]
       (if (> (count result) 0) result nil )
  )
)

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors)
)

(defn has-a-living-author? [book]
  (let [[result] [(living-authors (:authors book))]]
       (not (empty? result))
  )
)

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books)
)

; %________%
