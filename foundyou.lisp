(in-package :foundyou)

(defparameter *default-sensor* nil
  "This parameter should indicate whether or not the information for the geo-location request came
   from a sensor or not.  If it came from a sensor, it should be non-nil.  if it did not come from
   a sensor, it should be nil.")

(defparameter *default-use-https-p* nil
  "Set this parameter to a non-nil value if you want to default to using https, instead of http for
   the requests to google.")

(defun call-google (&key
		      (use-https-p *default-use-https-p*)
		      (sensor *default-sensor*)
		      address latitude longitude components)
  "Sends a call to google and returns the response, as parsed by JSOWN.
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
                  reverse geocoding."
  (assert (alexandria:xor (or address components) (and latitude longitude))
	  (address components latitude longitude)
	  "Must supply either latitude AND longitude, or address AND/OR components.")
  (assert (not (alexandria:xor latitude longitude))
	  (latitude longitude)
	  "When you supply either latitude or longitude, you must supply both.")
  (let ((parameters `(("sensor" . ,(if sensor "true" "false")))))
    (when address
      (push (cons "address" address) parameters))
    (when latitude
      (push (cons "lat" latitude) parameters))
    (when longitude
      (push (cons "lng" longitude) parameters))
    (when components
      (push (cons "lng"
		  (if (stringp components)
		      components
		      (format nil "~{~A:~A~^|~}" (loop for k in (jsown:keywords components)
						    append (list k (jsown:val components k))))))
	    parameters))
    (jsown:parse
     (flexi-streams:octets-to-string
      (drakma:http-request (format nil "http~:[~;s~]://maps.googleapis.com/maps/api/geocode/json"
				   use-https-p)
			   :parameters parameters)))))
