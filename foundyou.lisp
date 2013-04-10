(in-package :foundyou)

(defconstant +rooftop+ 10
  "The numeric values coupled to location_type ROOFTOP. see #'geocode.")
(defconstant +range-interpolated+ 9
  "The numeric values coupled to location_type RANGE_INTERPOLATED. see #'geocode.")
(defconstant +geometric-center+ 5
  "The numeric values coupled to location_type GEOMETRIC_CENTER. see #'geocode.")
(defconstant +approximate+ 3
  "The numeric values coupled to location_type APPROXIMATE. see #'geocode.")
(defconstant +unknown+ 0
  "The numeric values coupled to an unknown location_type. see #'geocode.")

(defparameter *default-sensor* nil
  "This parameter should indicate whether or not the information for the geo-location request came
   from a sensor or not.  If it came from a sensor, it should be non-nil.  if it did not come from
   a sensor, it should be nil.")

(defparameter *default-use-https-p* nil
  "Set this parameter to a non-nil value if you want to default to using https, instead of http for
   the requests to Google.")

(defparameter *default-bounds* nil
  "The default bounds to use when performing requests")

(defparameter *default-language* nil
  "The default language to use when performing requests")

(defparameter *default-region* nil
  "The default region to use when performing requests")

(define-condition google-error (error)
  ()
  (:documentation "Superclass of all Google Maps errors."))
(define-condition zero-results (google-error)
  ()
  (:documentation "Indicates that the geocode was successful but returned no results. This may occur
                   if the geocode was passed a non-existent address or a latlng in a remote
                   location."))
(define-condition over-query-limit (google-error)
  ()
  (:documentation "Indicates that you are over your quota."))
(define-condition request-denied (google-error)
  ()
  (:documentation "Indicates that your request was denied, generally because of lack of a sensor
                   parameter."))
(define-condition invalid-request (google-error)
  ()
  (:documentation "Generally indicates that the query (address or latlng) is missing."))
(define-condition unknown-error (google-error)
  ()
  (:documentation "Indicates that the request could not be processed due to a server error. The
                   request may succeed if you try again."))
(define-condition unknown-status-error (google-error)
  ()
  (:documentation "Indicates we received a non-\"OK\" status but with an error not currently
                   described in the Google Maps V3 API."))

(defun call-google-v3 (&key
			 (use-https-p *default-use-https-p*)
			 (sensor *default-sensor*)
			 address latitude longitude components
			 (bounds *default-bounds*)
			 (language *default-language*)
			 (region *default-region*))
  "Sends a call to Google and returns the response, as parsed by JSOWN.
   You must supply either latitude AND longitude, or address AND/OR components.  Nil-values are
   considered as not supplying the value.
   - address :: should be a string describing the address.  used when geocoding.
   - components :: describes the filtering which should occur when geocoding locations.
                   should contain either a string, as specified by
                   https://developers.google.com/maps/documentation/geocoding/?hl=en#ComponentFiltering
                   or a JSOWN object with the following keys, and strings as their values
                   (snippet taken from the link above):
                   - route matches long or short name of a route.
                   - locality matches against both locality and sublocality types.
                   - administrative_area matches all the administrative_area levels.
                   - postal_code matches postal_code and postal_code_prefix.
                   - country matches a country name or a two letter ISO 3166-1 country code.
   - latitude :: should contain the latitude, either as a string, or as a number.  used when
                 reverse geocoding.
   - longitude :: should contained the longitude, either as a string, or as a number.  used when
                  reverse geocoding.
   the documentation for bounds, language and region is mostly copied from the API at
   https://developers.google.com/maps/documentation/geocoding/?hl=en#GeocodingRequests
   they are optional
   - bounds :: The bounding box of the viewport within which to bias geocode results more
               prominently. This parameter will only influence, not fully restrict, results from the
               geocoder.  You may supply this as a string, or as (list lat-ne lng-ne lat-sw lng-sw)
               with the following meaning:
               - lat-ne :: number containing northeast boundary latitude
               - lng-ne :: number containing northeast boundary longitude
               - lat-sw :: number containing southwest boundary latitude
               - lng-sw :: number containing southwest boundary longitude
   - language :: The language in which to return results. See the list of supported domain
                 languages. Note that we often update supported languages so this list may not be
                 exhaustive. If language is not supplied, the geocoder will attempt to use the
                 native language of the domain from which the request is sent wherever possible.
   - region :: The region code, specified as a ccTLD (\"top-level domain\") two-character value.
               This parameter will only influence, not fully restrict, results from the geocoder."
  (assert (alexandria:xor (or address components) (and latitude longitude))
	  (address components latitude longitude)
	  "Must supply either latitude AND longitude, or address AND/OR components.")
  (assert (not (alexandria:xor latitude longitude))
	  (latitude longitude)
	  "When you supply either latitude or longitude, you must supply both.")
  (let ((parameters `(("sensor" . ,(if sensor "true" "false")))))
    (when address
      (push (cons "address" address) parameters))
    (when (and latitude longitude)
      (push (cons "latlng" (format nil "~,10F,~,10F" latitude longitude)) parameters))
    (when components
      (push (cons "lng"
		  (if (stringp components)
		      components
		      (format nil "~{~A:~A~^|~}" (loop for k in (jsown:keywords components)
						    append (list k (jsown:val components k))))))
	    parameters))
    (when bounds
      (push (cons "bounds"
		  (if (stringp bounds)
		      bounds
		      (format nil "~{~F,~F~^|~}" bounds)))
	    parameters))
    (when language
      (push (cons "language" language) parameters))
    (when region
      (push (cons "region" region) parameters))
    (let* ((http-target (format nil "http~:[~;s~]://maps.googleapis.com/maps/api/geocode/json"
				use-https-p))
	   (response
	    (jsown:parse
	     (flexi-streams:octets-to-string
	      (drakma:http-request http-target
				   :parameters parameters
				   :external-format-out :utf8
				   :external-format-in :utf8)
	      :external-format :utf8)))
	   (response-status (jsown:val response "status")))
      (cond ((string= response-status "ZERO_RESULTS")
	     (error 'zero-results))
	    ((string= response-status "OVER_QUERY_LIMIT")
	     (error 'over-query-limit))
	    ((string= response-status "REQUEST_DENIED")
	     (error 'request-denied))
	    ((string= response-status "INVALID_REQUEST")
	     (error 'invalid-request))
	    ((string= response-status "UNKNOWN_ERROR")
	     (error 'unknown-error))
	    ((string/= response-status "OK")
	     (error 'unknown-status-error)))
      response)))

(defun geocode (search-string &rest args
		&key
		  ;; keywords only listed for code hints
		  (use-https-p *default-use-https-p*)
		  (sensor *default-sensor*)
		  components
		  (bounds *default-bounds*)
		  (language *default-language*)
		  (region *default-region*))
  "Geocodes <address>, a string.
   Returns (values coords specificity formatted-address result-jsown other-jsown-results) if a
   result was found, or nil if no result was found.
   Documentation on each of the accepted keys can be found in the docstring for #'call-google-v3.
   - coords :: a list with the latitude as the first, and the longitude as the second element.
               both latitude and longitude are numbers.
   - specificity :: A number from 0 to 10 indicating how good the result was.  This is based on the
                    \"location_type\" keyword in the result.  10 means the address was found
                    exactly, 1 means the address is very vague.  0 means we don't quite know what
                    Google is trying to tell us.  We use numbers to make comparison easier.  The
                    numbers which we use for Google's terms are stored in the constants +rooftop+,
                    +range-interpolated+, +geometric-center+ and +approximate+.  If Google supplied
                    a location_type unknown to us, the specificity is 0 (or +specificity-unknown+).
   - formatted-address :: The found location, as formatted by Google.  Handy for cleaning up user-
                          supplied locations.
   - result-jsown :: This value contains the resulting jsown object from which we parse the
                     necessary information.  It contains some extras such as address_components
                     which didn't seem that important to us.  If you don't mind tying yourself to
                     the Google Maps API v3, you may use this to gain more information.
                     What is in here is described in the first element of \"results\" of
                     https://developers.google.com/maps/documentation/geocoding/?hl=en#JSON
   - other-jsown-results :: In most cases there will be a single result matching your geocode
                            request.  However, Google indicates that it may return more than one
                            result if the request was ambiguous.  The list of other results is
                            returned in this value.  Each of the elements in the list is formatted
                            in the same way as result-jsown.  The function #'decode-geocode-result
                            may help you in interpreting these objects.  It is NOT exported from
                            this package to reduce confusion."
  (declare (ignore use-https-p sensor components bounds language region))
  (let ((res (apply #'call-google-v3 :address search-string args)))
    (apply #'values
	   `(,@(decode-geocode-result (first (jsown:val res "results")))
	     ,(first (jsown:val res "results"))
	     ,(rest (jsown:val res "results"))))))

(defun specificity-api-string-to-number (string)
  "returns the number which corresponds to the specificity in <string>."
  (cond ((string= string "ROOFTOP") +rooftop+)
	((string= string "RANGE_INTERPOLATED") +range-interpolated+)
	((string= string "GEOMETRIC_CENTER") +geometric-center+)
	((string= string "APPROXIMATE") +approximate+)
	(t +unknown+)))

(defun decode-geocode-result (jsown-geocoded-address)
  "This is a helper to decode the results which Google supplies us when geocoding an address.
   Decodes the geocoded JSOWN address <jsown-geocoded-address> and returns (list coords specificity
   formatted-address). The meaning of these values is described in #'geocode."
  (let ((geo (jsown:val jsown-geocoded-address "geometry")))
    (list (list (jsown:filter geo "location" "lat")
		(jsown:filter geo "location" "lng"))
	  (specificity-api-string-to-number
	   (jsown:val geo "location_type"))
	  (jsown:val jsown-geocoded-address "formatted_address"))))

(defun reverse-geocode (latitude longitude &rest args
			&key
			  ;; keywords only listed for code hints
			  (use-https-p *default-use-https-p*)
			  (sensor *default-sensor*)
			  (bounds *default-bounds*)
			  (language *default-language*)
			  (region *default-region*))
  "Retrieves the address for a geo coordinate.  This is essentially the inverse of geocode.
   - latitude :: latitude, as a number.
   - longitude :: longitude, as a number.
   Documentation about the keywords is listed in the docstring of #'call-google-v3.
   Returns (values formatted-address address-plist address-short-plist
                   result-jsown other-jsown-results).
   - address-plist :: Plist which contains the following parsed information from <result-jsown>.
                      Much of the following documentation is taken from the Google Maps V3 API
                      documentation.  The mentioned name is the lisp keyword in the getf-able plist,
                      following that is Google's documentation between brackets.  Not all possible
                      information is captured.  The information captured in this plist is the long
                      name.  The same information, in short-form is captured in address-plist-short.
         - street-number :: [street_address] indicates a precise street address.
         - street-name :: [route] indicates a named route (such as \"US 101\").
         - country :: [country] indicates the national political entity, and is typically the
                      highest order type returned by the Geocoder.
         - area-level-1 :: [administrative_area_level_1] indicates a first-order civil entity
                           below the country level. Within the United States, these administrative
                           levels are states. Not all nations exhibit these administrative levels.
         - area-level-2 :: [administrative_area_level_2] indicates a second-order civil entity below
                           the country level. Within the United States, these administrative levels 
                           are counties. Not all nations exhibit these administrative levels.
         - area-level-3 :: [administrative_area_level_3] indicates a third-order civil entity below
                           the country level. This type indicates a minor civil division. Not all
                           nations exhibit these administrative levels.
         - city :: [locality] indicates an incorporated city or town political entity.
         - postal-code :: [postal_code] indicates a postal code as used to address postal mail
                          within the country.
   - address-plist-short :: the same basic information as address-plist, though here the short names
                            are contained in the plist (vs the long names in address-plist).
   - formatted-address :: A string, representing the address in a formatted form for the first
                          address returned by Google.
   - result-jsown :: Jsown representation of the first address returned by Google.  This is object
                     contains all raw information and can be used in conjunction with the Google
                     Maps V3 API.  See
                     https://developers.google.com/maps/documentation/geocoding/?hl=en#Types for
                     more information on its contents.
   - other-jsown-results :: Unparsed version of the following found addresses for the given
                            location.  This will be nil in almost all practical cases.  You may find
                            the unexported function #'decode-reverse-geocode-result handy when
                            handling these results."
  (declare (ignore use-https-p sensor bounds language region))
  (let ((res (apply #'call-google-v3 :latitude latitude :longitude longitude args)))
    (apply #'values
	   `(,@(decode-reverse-geocode-result (first (jsown:val res "results")))
	     ,(first (jsown:val res "results"))
	     ,(rest (jsown:val res "results"))))))

(defparameter *address-component-name-map*
  (alexandria:plist-hash-table '("street_address" :street-number
				 "street_number" :street-number
				 "route" :street-name
				 "country" :country
				 "administrative_area_level_1" :area-level-1
				 "administrative_area_level_2" :area-level-2
				 "administrative_area_level_3" :area-level-3
				 "locality" :city
				 "postal_code" :postal-code)
			       :test 'equal)
  "contains the mapping from Google's \"type\" tag in \"address_components\" to the lisp keyword we
   want to use to identify that component in the address-plist returned by reverse-geocode.")

(defun decode-reverse-geocode-result (jsown-reverse-geocoded-address)
  "This is a helper to decode the results which Google supplies us when reverse-geocoding an
   address.  Decodes the reverse-geocoded JSOWN address <jsown-reverse-geocoded-address> and
   returns (list address-plist address-plist-short formatted-address) as described in
   #'reverse-geocode."
  (let (address-plist
	address-plist-short)
    (dolist (component (jsown:val jsown-reverse-geocoded-address "address_components"))
      (dolist (type (jsown:val component "types"))
	(let ((keyword (gethash type *address-component-name-map*)))
	  (when keyword
	    (setf (getf address-plist keyword) (jsown:val component "long_name"))
	    (setf (getf address-plist-short keyword) (jsown:val component "short_name"))))))
    (list address-plist
	  address-plist-short
	  (jsown:val jsown-reverse-geocoded-address "formatted_address"))))
