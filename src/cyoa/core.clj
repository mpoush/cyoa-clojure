(ns cyoa.core
  (:require [clojure.java.io :as io])
  (:use clojure.set)
  (:use clojure.pprint))

(def settings {:image-extension "webp"
               :image-padding 3
               :force-in-order false
               :blur 10})

(defn to-filename [n ext] (str (format (str "%0" (:image-padding settings) "d.") n) ext))

(defn store-in
  [m k v]
  (let [dest (or (get m k) #{})]
    (into m {k (conj dest v)})))

(defn collect-nodes
  ([ls] (collect-nodes ls {}))
  ([[a b & remainder] dict]
   (let [d2 (store-in dict a b)]
     (if (empty? remainder)
       d2 (collect-nodes remainder d2)))))

(defn describe-page*
  [nodes targets n]
  (let [after (get nodes n)
        linked (contains? targets n)]
    {:page       n
     :after      (sort after)
     :is-image?  (and (not linked) (nil? after))
     :continues? (contains? after (+ n 1))
     :ends?      (and linked (nil? after))}))

(defn is-preserved?
  [n page in-split]
  (or
    (not in-split)
    (:is-image? page)
    (= n (:page page))))

(defn information-on
  [page pages]
  (let [n (:page page)
        [left right](if (even? n)
                      [page (get pages (inc n))]
                      [(get pages (dec n)) page])
        in-split (not (:continues? left))]
    {:page       n
     :left-img   (:page left)
     :left-blur  (and (not (is-preserved? n left in-split)))
     :right-img  (:page right)
     :right-blur (and (not (is-preserved? n right in-split)))
     :links      (if (:ends? page) '(1) (:after page))}))

(defn display-information
  ([pages] (display-information pages pages))
  ([remaining pages]
   (if (empty? remaining) []
                          (cons
                            (information-on (first remaining) pages)
                            (display-information (rest remaining) pages)))))

(defn get-pages
  [contents]
  (let [links (if (:force-in-order settings)
                (flatten (map #(identity [% (+ 1 %)]) (range 1 (- (:pages contents) 1))))
                (:links contents))
        nodes (collect-nodes links)
        targets (reduce union (set (keys nodes)) (vals nodes))
        describe-page (memoize (partial describe-page* nodes targets))
        printed-pages (range (+ 1 (:pages contents)))
        value (into [] (map describe-page printed-pages))
        result (into [] (display-information value))]
    (into {} (map #(identity {(:page %) %}) (filter #(contains? targets (:page %)) result)))))

(defn combine-links
  [links after]
  (sort (disj (set (concat links (:links after))) (:page after))))

(defn pages-overlap
  [page links after]
  (and (even? (:page page))
       (not (:left-blur page))
       (not (:right-blur page))
       (contains? (set links) (:page after))))

(defn squash-pages
  [pages page]
  (let [links (:links page)
        n (:page page)
        after (get pages (inc n))]
    (if (pages-overlap page links after)
      (or (assert (not (:left-blur after)) (str "Page " (+ n 1) " is hiding page" n))
          (assert (not (:right-blur after)) (str "Page " (+ n 1) " is hiding itself."))
          (into page {:links (combine-links links after)}))
      page)))

(defn to-references
  ([page] (to-references page (:image-extension settings)))
  ([page img-format]
   (into page {
               :left-img (to-filename (:left-img page) img-format)
               :right-img (to-filename (:right-img page) img-format)})))

(defn parse-string
  [contents]
  (let [pages (get-pages contents)]
    (map to-references (sort-by #(:page %) (map #(squash-pages pages %) (vals pages))))))

(defn html-image
  [path blurred]
  (let [style (if blurred (format " style=\"filter: blur(%dpx);\"" (:blur settings)) "")]
    (format "<img src=\"%s\"%s />" (io/file "images" path) style)))

(defn html-link
  [link]
  (format "<a href=\"%s\">Turn to page %s</a>" (to-filename link "html") link))

(defn save-html
  [data folder]
  (let [target (io/file folder (to-filename (:page data) "html"))
        html (str "<html>\r\n<head>\r\n"
                  "<title>Page %s</title>\r\n</head>\r\n"
                  "<body><p>\r\n%s\r\n%s\r\n</p><p>\r\n%s\r\n</p></body>\r\n</html>")]
    (io/make-parents target)
    (spit target (format html (:page data)
                         (html-image (:left-img data) (:left-blur data))
                         (html-image (:right-img data) (:right-blur data))
                         (clojure.string/join "<br/>\r\n" (map html-link (:links data)))))))

(defn -main
  [& filename]
  (let [book (or (next filename) "ufo_54_40.edn")
        contents (read-string (slurp (io/file "resources" book)))
        parsed (parse-string contents)
        output-folder (io/file "resources" (subs book 0 (- (count book) 4)))]
    (println (format "Exported %d pages" (count (map #(save-html % output-folder) parsed))))
    (spit (io/file output-folder "parsed.edn") (with-out-str (pprint parsed)))))
