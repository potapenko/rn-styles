(ns rn-styles.core
  (:require [reagent.core :as r :refer [atom]]
            [reagent.impl.component :as ru]))

(def react (js/require "react-native"))

(defn- create-styles [s]
  (let [s1 (reduce #(assoc %1 (%2 0) (ru/camelify-map-keys (%2 1))) {} s)]
    (.create (.-StyleSheet react) (clj->js s1))))

(def ^:private styles (atom {}))
(def ^:private original-styles (atom {}))

(defn- get-stylesheet [domain]
  (domain @styles))

(defn- add-stylesheet [domain new-styles]
  (if-let [exists (get-stylesheet domain)]
    exists
    (do
      (reset! styles (assoc @styles domain (create-styles new-styles)))
      (get-stylesheet domain))))

(defn- get-style [style-name]
  (when-let [sheet (get-stylesheet style-name)]
    (aget sheet (name style-name))))

(defn- new-style [style-name style]
  (if-let [exists (get-style style-name)]
    exists
    (do
      (add-stylesheet style-name (assoc {} style-name style))
      (let [style-id (get-style style-name)]
        (reset! original-styles (assoc @original-styles style-id style))
        style-id))))

(defn- original [style-id]
  (get @original-styles style-id))

(defn print-style
  "Утилитная функция - выводит на консоль стиль или список стилей в формате map"
  [& style]
  (println)
  (println)
  (println)
  (let [style-map    (apply merge (map original style))
        sorted-style (doall
                      (sort #(compare (name (:k %1)) (name (:k %2)))
                            (for [[k v] style-map]
                              {:k k :v (if (string? v) (str "\"" v "\"") v)})))]
    (println "(s/new-style :my-style-rename-this")
    (println "             {")
    (doseq [x sorted-style] (println "             " (:k x) (:v x)))
    (println "              })"))
  (println)
  (println)
  (println)
  (vec style))

; layout

(defn display
  "display sets the display type of this component.
  It works similarly to display in CSS, but only support 'flex' and 'none'. 'flex' is the default."
  [v]
  [:flex :none]
  (let [v (name v)]
    (new-style (keyword (str "display-" v)) {:display v})))

(defn flex
  "n React Native flex does not work the same way that it does in CSS. flex is a number rather than a string, and it works according to the Yoga library at https://github.com/facebook/yoga
  When flex is a positive number, it makes the component flexible and it will be sized proportional to its flex value. So a component with flex set to 2 will take twice the space as a component with flex set to 1.
  When flex is 0, the component is sized according to width and height and it is inflexible.
  When flex is -1, the component is normally sized according width and height. However, if there's not enough space, the component will shrink to its minWidth and minHeight.
  flexGrow, flexShrink, and flexBasis work the same as in CSS."
  ([] (flex 1))
  ([v]
   (new-style (keyword (str "flex-" v)) {:flex v})))

(defn align-self
  "alignSelf controls how a child aligns in the cross direction, overriding the alignItems of the parent. It works like align-self in CSS (default: auto). See https://developer.mozilla.org/en-US/docs/Web/CSS/align-self for more details."
  [v]
  (new-style (keyword (str "align-self-" v)) {:align-self v}))

(defn align-items
  "alignItems aligns children in the cross direction. For example, if children are flowing vertically, alignItems controls how they align horizontally. It works like align-items in CSS (default: stretch). See https://developer.mozilla.org/en-US/docs/Web/CSS/align-items for more details"
  [v]
  (new-style (keyword (str "align-items-" v)) {:align-items v}))

(defn flex-align
  "(common pattern) align-items plus justify-content"
  [v]
  (new-style (keyword (str "flex-align-" v)) {:align-items     v
                                              :justify-content v}))

(defn justify-content
  "justifyContent aligns children in the main direction. For example, if children are flowing vertically, justifyContent controls how they align vertically. It works like justify-content in CSS (default: flex-start). See https://developer.mozilla.org/en-US/docs/Web/CSS/justify-content for more details.

  enum('flex-start', 'flex-end', 'center', 'space-between', 'space-around', 'space-evenly')"
  [v]
  (new-style (keyword (str "justify-content-" v)) {:justify-content v}))

(defn flex-wrap
  "flexWrap controls whether children can wrap around after they hit the end of a flex container. It works like flex-wrap in CSS (default: nowrap). See https://developer.mozilla.org/en-US/docs/Web/CSS/flex-wrap for more details."
  [v]
  (new-style (keyword (str "flex-wrap-" v)) {:flex-wrap v}))

(defn flex-direction
  "flexDirection controls which directions children of a container go. row goes left to right, column goes top to bottom, and you may be able to guess what the other two do. It works like flex-direction in CSS, except the default is column. See https://developer.mozilla.org/en-US/docs/Web/CSS/flex-direction for more details."
  [v]
  (new-style (keyword (str "flex-direction-" v)) {:flex-direction v}))

(defn flex-basis
  ""
  [v]
  (new-style (keyword (str "flex-basis-" v)) {:flex-basis v}))

(defn flex-shink
  ""
  [v]
  (new-style (keyword (str "flex-shink-" v)) {:flex-shink v}))

(def row
  "(common pattern) flex-direction 'row'"
  (new-style :row {:flex-direction "row"}))
(def column
  "(common pattern) flex-direction 'column'"
  (new-style :column {:flex-direction "column"}))

(def align-center
  "(common pattern) align-items 'center'"
  (align-items "center"))

(def align-right
  "(common pattern) align-items 'flex-end'"
  (align-items "flex-end"))

(def stretch
  "(common pattern) align-items 'stretch'"
  (align-items "stretch"))

(def justify-center
  "(common pattern) justify-content 'center'"
  (justify-content "center"))

(def wrap
  "(common pattern) flex-wrap 'wrap'"
  (new-style :flex-wrap {:flex-wrap "wrap"}))

(def no-wrap
  "(common pattern) flex-wrap 'nowrap'"
  (new-style :flex-wrap {:flex-wrap "nowrap"}))

; background color

(defn gray-cl
  "Useful function for simple generate rgba(0,0,0,0.N) color."
  [opacity]
  (str "rgba(0,0,0,0." opacity ")"))

(defn gray
  "Useful function for generate style background-color 'rgba(0,0,0,0.N)'."
  ([] (gray 2))
  ([opacity]
   (new-style
    (keyword (str "gray-" opacity))
    {:background-color (gray-cl opacity)})))

(defn white-cl
  "Useful function for simple generate rgba(255,255,255,0.N) color."
  [opacity]
  (str "rgba(255,255,255,0." opacity ")"))

(defn white
  "Useful function for generate style background-color 'rgba(255,255,255,0.N)'."
  ([]  (white 999))
  ([opacity]
   (new-style
    (keyword  (str "white-" opacity))
    {:background-color (white-cl opacity)})))

;; text styles

(defn color
  "The color property specifies the color of text."
  [v]
  (new-style (keyword (str "color-" v)) {:color v}))

(defn font-style
  "enum('normal', 'italic'"
  [v]
  (new-style (keyword (str "font-style" v)) {:font-style v}))

(defn font-family
  "The font-family property specifies the font for an element."
  [v]
  (new-style (keyword (str "font-family" v)) {:font-family v}))

(defn font-weight
  "Specifies font weight. The values 'normal' and 'bold' are supported for most fonts. Not all fonts have a variant for each of the numeric values, in that case the closest one is chosen.

  enum('normal', 'bold', '100', '200', '300', '400', '500', '600', '700', '800', '900')"
  [v]
  (new-style (keyword (str "font-weight" v)) {:font-weight v}))

(defn line-height
  ""
  [v]
  (new-style (keyword (str "line-height" v)) {:line-height v}))

(defn text-align-vertical
  "(Android) enum('auto', 'top', 'bottom', 'center')"
  [v]
  (new-style (keyword (str "text-align-vertical" v)) {:text-align-vertical v}))

(defn font-variant
  "(IOS) array of enum('small-caps', 'oldstyle-nums', 'lining-nums', 'tabular-nums', 'proportional-nums')"
  [v]
  (new-style (keyword (str "font-variant-" v)) {:font-variant v}))

(defn leter-spacing
  "(IOS)"
  [v]
  (new-style (keyword (str "leter-spacing" v)) {:leter-spacing v}))

(defn text-decoration-color
  "(IOS)"
  [v]
  (new-style (keyword (str "text-decoration-color" v)) {:text-decoration-color v}))

(defn text-decoration-line
  "enum('none', 'underline', 'line-through', 'underline line-through')"
  [v]
  (new-style (keyword (str "text-decoration-line" v)) {:text-decoration-line v}))

(def underline
  "(common pattern) text-decoration undeline"
  (text-decoration-line "underline"))

(def line-through
  "enum('none', 'underline', 'line-through', 'underline line-through')"
  (text-decoration-line "line-through"))

(defn text-decoration-style
  "(IOS) enum('solid', 'double', 'dotted', 'dashed')"
  [v]
  (new-style (keyword (str "text-decoration-style" v)) {:text-decoration-style v}))

(defn writing-direction
  "(IOS) enum('auto', 'ltr', 'rtl')"
  [v]
  (new-style (keyword (str "writing-direction" v)) {:writing-direction v}))

(def bold
  "font-weight 'bold'"
  (font-weight "bold"))

(def italic
  "font-weight 'italic'"
  (font-style "italic"))

(defn font-size
  ""
  [v]
  (new-style (keyword (str "font-size-" v)) {:font-size v}))

(defn text-align
  "Specifies text alignment. The value 'justify' is only supported on iOS and fallbacks to left on Android.

  enum('auto', 'left', 'right', 'center', 'justify')"
  [v]
  (new-style (keyword (str "text-align-" v)) {:text-align v}))

(defn text-shadow
  "Drop shadow for text."
  ([] (text-shadow 0 2 5 2))
  ([w h r a]
   (new-style (keyword (str "text-shadow-" a "-" w "-" h "-" r))
              {:text-shadow-offset {:width w :height h}
               :text-shadow-radius r
               :text-shadow-color  (str "rgba(0,0,0,0." a ")")})))

(defn include-font-padding
  "(Android) Set to false to remove extra font padding intended to make space for certain ascenders / descenders. With some fonts, this padding can make text look slightly misaligned when centered vertically. For best results also set textAlignVertical to center. Default is true."
  [v]
  (new-style (keyword (str "include-font-padding-" v)) {:include-font-padding v}))

;; dimentions

(defn height
  "height sets the height of this component.
  It works similarly to height in CSS, but in React Native you must use points or percentages. Ems and other units are not supported. "
  [v]
  (new-style (keyword (str "height-" v)) {:height v}))

(defn min-height
  "minHeight is the minimum height for this component, in logical pixels.
  It works similarly to min-height in CSS, but in React Native you must use points or percentages. Ems and other units are not supported."
  [v]
  (new-style (keyword (str "min-height-" v)) {:min-height v}))

(defn max-height
  "maxHeight is the maximum height for this component, in logical pixels.
  It works similarly to max-height in CSS, but in React Native you must use points or percentages. Ems and other units are not supported."
  [v]
  (new-style (keyword (str "max-height-" v)) {:max-height v}))

(defn width
  "width sets the width of this component.
  It works similarly to width in CSS, but in React Native you must use points or percentages. Ems and other units are not supported. "
  [v]
  (new-style (keyword (str "width-" v)) {:width v}))

(defn min-width
  "minWidth is the minimum width for this component, in logical pixels.
  It works similarly to min-width in CSS, but in React Native you must use points or percentages. Ems and other units are not supported."
  [v]
  (new-style (keyword (str "min-width-" v)) {:min-width v}))

(defn max-width
  "maxWidth is the maximum width for this component, in logical pixels.
  It works similarly to max-width in CSS, but in React Native you must use points or percentages. Ems and other units are not supported."
  [v]
  (new-style (keyword (str "max-width-" v)) {:max-width v}))

(defn size
  "Setting size is like setting both of height and width."
  [w h]
  (new-style (keyword (str "width-" w "-height-" h)) {:width w :height h}))

(defn layout->size
  "It converts layout props {:height N :widht N} to style."
  [layout]
  (new-style (keyword (str "width-" (:widht layout) "-height-" (:height layout)))
             (select-keys layout [:width :height])))

;; borders

(defn rounded
  "Rounded corners for view."
  ([] (rounded 8))
  ([v]
   (new-style (keyword (str "rounded-" v))
              {:border-radius v}))
  ([tl bl] (rounded tl bl tl bl))
  ([tr br bl tl]
   (new-style (keyword (str "rounded-" tr "-" br "-" bl "-" tl))
              {:border-top-right-radius    tr
               :border-bottom-right-radius br
               :border-bottom-left-radius  bl
               :border-top-left-radius     tl})))

(defn border
  "Settings for view borders"
  ([] (border 1))
  ([w] (border w "rgba(0,0,0,0.4)"))
  ([w c] (border w c "solid"))
  ([w c s]
   (new-style (keyword (str "border-" w "-" c "-" (name s)))
              {:border-width w
               :border-style (name s)
               :border-color c})))

(defn border-bottom
  ""
  ([] (border-bottom 1))
  ([w] (border-bottom w "rgba(0,0,0,0.4)"))
  ([w c]
   (new-style (keyword (str "border-bottom-" w "-" c))
              {:border-bottom-width w
               :border-bottom-color c})))

(defn border-top
  ""
  ([] (border-top 1))
  ([w] (border-top w "rgba(0,0,0,0.4)"))
  ([w c]
   (new-style (keyword (str "border-top-" w "-" c))
              {:border-top-width w
               :border-top-color c})))

(defn border-left
  ""
  ([] (border-left 1))
  ([w] (border-left w "rgba(0,0,0,0.4)"))
  ([w c]
   (new-style (keyword (str "border-left-" w "-" c))
              {:border-left-width w
               :border-left-color c})))

(defn border-right
  ""
  ([] (border-right 1))
  ([w] (border-right w "rgba(0,0,0,0.4)"))
  ([w c]
   (new-style (keyword (str "border-right-" w "-" c))
              {:border-right-width w
               :border-right-color c})))

(defn elevation
  "(Android) Sets the elevation of a view, using Android's underlying elevation API.
  This adds a drop shadow to the item and affects z-order for overlapping views.
  Only supported on Android 5.0+, has no effect on earlier versions."
  [v]
  (new-style (keyword (str "elevation-" v)) {:elevation v}))

(defn shadow
  "Settings drop shadow"
  ([] (shadow 4))
  ([h] (shadow 0 h))
  ([w h] (shadow w h h))
  ([w h r] (shadow w h r 4))
  ([w h r a]
   (new-style (keyword (str "text-shadow-" a "-" w "-" h "-" a))
              {:shadow-opacity 1
               :shadow-offset  {:width w :height h}
               :shadow-radius  r
               :shadow-color   (str "rgba(0,0,0,0." a ")")
               :elevation      h})))


;; box

(defn padding
  "setting padding has the same effect as setting each of paddingtop, paddingbottom, paddingleft, and paddingright."
  ([] (padding 8))
  ([v] (padding v v))
  ([t r] (padding t r t r))
  ([t r b l]
   (new-style (keyword (str "padding-" t "-" r "-" b "-" l))
              {:padding-top    t
               :padding-right  r
               :padding-bottom b
               :padding-left   l})))

(defn padding-horizontal
  "Setting paddingHorizontal is like setting both of paddingLeft and paddingRight."
  [v]
  (new-style (keyword (str "padding-horizontal-" v)) {:padding-horizontal v}))

(defn padding-vertical
  "Setting paddingVertical is like setting both of paddingTop and paddingBottom."
  [v]
  (new-style (keyword (str "padding-vertical-" v)) {:padding-vertical v}))

(defn margin-horizontal
  "Setting paddingHorizontal is like setting both of paddingLeft and paddingRight."
  [v]
  (new-style (keyword (str "margin-horizontal-" v)) {:margin-horizontal v}))

(defn margin-vertical
  "Setting marginVertical has the same effect as setting both marginTop and marginBottom."
  [v]
  (new-style (keyword (str "margin-vertical-" v)) {:margin-vertical v}))

(defn margin
  "Setting margin has the same effect as setting each of marginTop, marginLeft, marginBottom, and marginRight. "
  ([] (margin 8))
  ([v] (margin v v))
  ([t r] (margin t r t r))
  ([t r b l]
   (new-style (keyword (str "margin-" t "-" r "-" b "-" l))
              {:margin-top    t
               :margin-right  r
               :margin-bottom b
               :margin-left   l})))

(defn margin-top
  "Works like margin-top in CSS."
  [v]
  (new-style (keyword (str "margin-top-" v)) {:margin-top v}))

(defn margin-left
  "Works like margin-left in CSS."
  [v]
  (new-style (keyword (str "margin-left-" v)) {:margin-left v}))

(defn margin-bottom
  "Works like margin-bottom in CSS."
  [v]
  (new-style (keyword (str "margin-bottom-" v)) {:margin-bottom v}))

(defn margin-right
  "Works like margin-right in CSS."
  [v]
  (new-style (keyword (str "margin-right-" v)) {:margin-right v}))

(defn padding-top
  "Works like padding-top in CSS."
  [v]
  (new-style (keyword (str "padding-top-" v)) {:padding-top v}))

(defn padding-left
  "Works like padding-left in CSS."
  [v]
  (new-style (keyword (str "padding-left-" v)) {:padding-left v}))

(defn padding-bottom
  "Works like padding-bottom in CSS."
  [v]
  (new-style (keyword (str "padding-bottom-" v)) {:padding-bottom v}))

(defn padding-right
  "Works like padding-right in CSS."
  [v]
  (new-style (keyword (str "padding-right-" v)) {:padding-right v}))

;; position

(defn position
  "position in React Native is similar to regular CSS, but everything is set to relative by default, so absolute positioning is always just relative to the parent.
  If you want to position a child using specific numbers of logical pixels relative to its parent, set the child to have absolute position.
  If you want to position a child relative to something that is not its parent, just don't use styles for that. Use the component tree."
  [v]
  (new-style (keyword (str "position-" v)) {:position v}))

(def position-absolute
  "(common pattern) position 'absolute'"
  (position "absolute"))

(defn top
  "top is the number of logical pixels to offset the top edge of this component.
  It works similarly to top in CSS, but in React Native you must use points or percentages. Ems and other units are not supported."
  [v]
  (new-style (keyword (str "top-" v)) {:top v}))

(defn left
  "left is the number of logical pixels to offset the left edge of this component.
  It works similarly to left in CSS, but in React Native you must use points or percentages. Ems and other units are not supported."
  [v]
  (new-style (keyword (str "left-" v)) {:left v}))

(defn bottom
  "bottom is the number of logical pixels to offset the bottom edge of this component.
  It works similarly to bottom in CSS, but in React Native you must use points or percentages. Ems and other units are not supported."
  [v]
  (new-style (keyword (str "bottom-" v)) {:bottom v}))

(defn right
  "right is the number of logical pixels to offset the right edge of this component.
  It works similarly to right in CSS, but in React Native you must use points or percentages. Ems and other units are not supported."
  [v]
  (new-style (keyword (str "right-" v)) {:right v}))

(def box
  "A very common pattern is to create overlays with position absolute and zero positioning."
  [(top 0) (left 0) (bottom 0) (right 0) position-absolute])

(defn z-index
  "zIndex controls which components display on top of others. Normally, you don't use zIndex. Components render according to their order in the document tree, so later components draw over earlier ones. zIndex may be useful if you have animations or custom modal interfaces where you don't want this behavior."
  [v]
  (new-style (keyword (str "z-index-" v)) {:z-index v}))

;; transform

(defn transform
  ""
  [v]
  (new-style (keyword (str "transform-" v)) {:transform v}))

;; view

(defn overflow
  ""
  [v]
  (new-style (keyword (str "overflow-" v)) {:overflow v}))

(defn opacity
  ""
  [v]
  (new-style (keyword (str "opacity-" v)) {:opacity v}))

(defn border-width
  ""
  [v] ; enum('visible', 'hidden')
  (new-style (keyword (str "border-width-" v)) {:border-width v}))

(defn border-style
  "enum('solid', 'dotted', 'dashed')"
  [v]
  (new-style (keyword (str "border-style-" v)) {:border-style v}))

(defn backface-visibility
  ""
  [v] ;enum('visible', 'hidden')
  (new-style (keyword (str "backface-visibility-" v)) {:backface-visibility v}))

(defn background-color
  ""
  [v]
  (new-style (keyword (str "background-color-" v)) {:background-color v}))

(defn border-top-color
  ""
  [v]
  (new-style (keyword (str "border-top-color-" v)) {:border-top-color v}))

(defn border-right-color
  ""
  [v]
  (new-style (keyword (str "border-right-color-" v)) {:border-right-color v}))

(defn border-bottom-color
  ""
  [v]
  (new-style (keyword (str "border-bottom-color-" v)) {:border-bottom-color v}))

(defn border-left-color
  ""
  [v]
  (new-style (keyword (str "border-left-color-" v)) {:border-left-color v}))

(defn border-top-width
  ""
  [v]
  (new-style (keyword (str "border-top-width-" v)) {:border-top-width v}))

(defn border-right-width
  ""
  [v]
  (new-style (keyword (str "border-right-width-" v)) {:border-right-width v}))

(defn border-bottom-width
  ""
  [v]
  (new-style (keyword (str "border-bottom-width-" v)) {:border-bottom-width v}))

(defn border-left-width
  ""
  [v]
  (new-style (keyword (str "border-left-width-" v)) {:border-left-width v}))

(defn border-bottom-left-radius
  ""
  [v]
  (new-style (keyword (str "border-bottom-left-radius-" v)) {:border-bottom-left-radius v}))

(defn border-bottom-right-radius
  ""
  [v]
  (new-style (keyword (str "border-bottom-right-radius-" v)) {:border-bottom-right-radius v}))

(defn border-top-left-radius
  ""
  [v]
  (new-style (keyword (str "border-top-left-radius-" v)) {:border-top-left-radius v}))

(defn border-top-right-radius
  ""
  [v]
  (new-style (keyword (str "border-top-right-radius-" v)) {:border-top-right-radius v}))

;; image

(defn border-color
  ""
  [v]
  (new-style (keyword (str "border-color-" v)) {:border-color v}))

(defn border-radius
  ""
  [v]
  (new-style (keyword (str "border-radius" v)) {:border-radius v}))

(defn resize-mode
  ""
  [v]
  (new-style (keyword (str "resize-mode" v)) {:resize-mode v}))

(defn overlay-color
  ""
  [v]
  (new-style (keyword (str "overlay-color" v)) {:overlay-color v}))

(defn tint-color
  ""
  [v]
  (new-style (keyword (str "tint-color" v)) {:tint-color v}))

(defn direction
  "(IOS) direction specifies the directional flow of the user interface.
  The default is inherit, except for root node which will have value based on the current locale. "
  [v]
  (new-style (keyword (str "direction" v)) {:direction v}))

;; utils

(defn circle
  "Creats circle view."
  ([radius] (circle radius nil 0 0))
  ([radius background ] (circle radius background 0 0))
  ([radius background border-radius border-color]
   (let [w (* 2 radius)]
     [align-center justify-center
      (overflow "hidden")
      (width w)
      (height w)
      (when background (background-color background))
      (rounded (/ w 2))
      (cond (and border-color border-radius) (border border-radius border-color)
            border-radius (border border-radius))])))

(def rotate-45
  ""
  (transform [{:rotate :45deg}]))
(def rotate-90
  ""
  (transform [{:rotate :90deg}]))
(def rotate-180
  ""
  (transform [{:rotate :180deg}]))
(def rotate-270 (transform [{:rotate :270deg}]))

(defn merge-styles
  ""
  [style-name & styles]
  (map-indexed (fn [index style]
                 (if (map? style)
                   (new-style (keyword (str style-name "-" index)) style)
                   style)
                 ) styles))
