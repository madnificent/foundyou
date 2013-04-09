(asdf:defsystem :foundyou
  :serial t
  :description "Interface to the Google geocode v3 api which allows for geocoding (find the latitude and longitude for a specific address), and reverse geocoding (find the address for a specific latitude and longitude)."
  :author "Aad Versteden <aad@knowified.com>"
  :depends-on (jsown drakma)
  :license "MIT"
  :components ((:file "package")
	       (:file "foundyou")))
