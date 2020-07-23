;;; bisq-api-public.el --- Bisq Public API for elisp     -*- lexical-binding: t -*-

;; Copyright (C) 2020 Deus Max

;; Author: Deus Max <deusmax@gmx.com>
;; URL: https://github.com/deusmax/gnus-notes
;; Version: 0.3.1
;; Package-Requires: ((emacs "25.1") (bbdb "3.1") (helm "3.1") (hydra "0.13.0") (org "8.3") (s "0.0") (lv "0.0") (async "1.9.1"))
;; Keywords: convenience, mail, bbdb, gnus, helm, org, hydra

;; This file is not part of GNU Emacs.

;;; License

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;

;;; To use:

;;; Code:
(require 'subr)
(require 'cl-lib)
(require 'request)

(defgroup bisq-api-public nil
  "Bisq Api Public for emacs-lisp."
  :tag "Bisq Api Public"
  :group 'extensions)

(defcustom bisq-api-public-url "https://markets.bisq.network/api/"
  "Base url for the Bisq public API, with the trailing slash."
  :group 'bisq-api-public
  :type 'string)

(defconst bisq-api-public-actions '("currencies" "depth" "hloc"
                                    "markets" "offers" "ticker"
                                    "trades" "volumes"))

(defvar bisq-api-public-last-request-response nil
  "Variable to save the last request-response.")

(defvar bisq-api-public-markets-ofinterest
  '(
    btc_eur  btc_usd            ; euro  usdollar
    xmr_btc  bsq_btc  zec_btc   ; monero bisque zcash 
    dash_btc fair_btc eth_btc   ; dash faircoin ethereum 
    ltc_btc  etc_btc  dcr_btc ) ; litecoin ethereum-classic decred
  "List of Bisq markets of interest.
To be modified according to user preferences. May be used to
limit or automate some actions. Currently not used.")

(defvar bisq-api-public--data '((currencies) (depth) (hloc) (markets)
                                (offers) (ticker) (trades) (volumes))
  "Variable for saving the returned data from each action. Adds
persistance to avoid repeated remote calls to the API. A
convenience interface for accessing these saved values is
pending.")

(defvar bisq-api-public-status-codes-alist
  '((200 . (lambda (&rest _) (message "Got OK.")))
    (400 . (lambda (&rest _) (message "Got 400.")))
    (418 . (lambda (&rest _) (message "Got 418."))))
  "An alist to be used for displaying a message depending on the http request status
  codes. Common values only, not exhaustive.")

(cl-defun bisq-api-public--error (&rest args &key error-thrown &allow-other-keys)
  "Function to run in case or request error.
Print a message with the ERROR-THROWN. Rest of ARGS not used."
  (message "Got error: %S" error-thrown))

(cl-defun bisq-api-public--complete (&rest _)
  "Display a brief message when the API request has completed."
  (message "Request on Bisq API finished!"))

(defun bisq-api-public-set-alist (key val alist)
  "Add KEY with VALUE to ALIST for use in request parameters.
Add key, value pair to the request parameters, skipping over the
default or nil values." )

;; See if this can be useful
;; (cl-defstruct bisq-api-public-response (:includes request-response))

(cl-defun bisq-api-public--request (url &key params (success (lambda (&rest _))) (result-type 'elisp) sync &allow-other-keys)
  "Internal function to create and send the requests to the Bisq API URL.
Calls directly the `request' function.

Function arguments are specified using the common-lisp keyword style.

URL: the url to send the request.
PARAMS: is an alist of (key . value) pairs to set the query part of the url.
SUCCESS: function to be called upon success, when running in async mode.
         Ignored otherwise.
RESULT-TYPE: Parse the returned json into elisp-type using `json-read'.
             Set to 'string or `t' for result type to be string.
SYNC: If `t', wait until request is done.  Default is `nil'. Sync should be used
      *only* for testing or debugging."
  (request url
    :params params
    :parser (when (cl-find result-type '(elisp emacs-lisp lisp) :test #'string=) #'json-read)
    :sync sync
    :success (if sync (lambda (&rest _)) success)
    :error    #'bisq-api-public--error
    :complete #'bisq-api-public--complete
    :status-code bisq-api-public-status-codes-alist))

(cl-defun bisq-api-public-currencies (&key (basecurrency "BTC")
                                           (type "all")
                                           (result-type 'elisp)
                                           jsonpretty
                                           sync onsuccess params)
  "Get list of available currencies for a given BASECURRENCY.
Calls the Bisq API currencies action. Depending on the request
options, the returned request data are a json string or parsed
to elisp-types (default).

Argument keywords, are optional, default values are used if
missing. Argument keywords are:

BASECURRENCY: base currency identifier, default \"BTC\".
TYPE: Type of currencies to inclued in results.
      Values crypto, fiat, all, default all.
JSONPRETTY: non-nil for pretty formatted json, defaul `nil'.
RESULT-TYPE: set to 'string to obtain the result as a json string,
             otherwise returned json is parsed to elisp with `json-read'.
SYNC: set `t' for synchronous request. Default is `nil' (async).
ONSUCCESS: user supplied function to be called on success, for
           async requests only. Ignored when running synchronous
           requests.
PARAMS: initializes an empty alist, for collecting the API request
        parameters. Not for use by users.

Return values depend on request running mode.
In async mode (default) will:
 1. save the `request-response' object to
    `bisq-api-last-request-response'. Regular request-response-*
    accessors can be applied to this object to obtain other
    information. See the documentation on `request.el'.
 2. save the returned request data in `bisq-api-public--data' to
    the cons cell with the first element named for the API action.

In synchronous mode (sync `t') will wait for the action to
complete and return the request result data, which are not
saved to `bisq-api-public--data'. `request-response' object
is saved to `bisq-api-last-request-response'.

Note on the basecurrency parameter from the Bisq API documentation:
   \"In practice, the same currencies are available for every
   basecurrency so this API can be called just once, omitting the
   basecurrency parameter.\"  "
  (unless jsonpretty (push (cons "format" "json") params))
  (unless (string= basecurency "BTC") (push (cons "basecurrency" basecurrency) params))
  (when (cl-find type '(crypto fiat) :test #'string=) (push (cons "type" type) params))
  (setq bisq-api-public-last-request-response
        (bisq-api-public--request
         (concat bisq-api-public-url "currencies/")
         :params params
         :result-type result-type
         :sync sync
         :success (cl-function
                   (lambda (&key data response &allow-other-keys)
                     (setf (alist-get 'currencies bisq-api-public--data) data)
                     (when (functionp onsuccess)
                       (funcall onsuccess :data data :response response))))))
  (when sync (request-response-data bisq-api-public-last-request-response)))

(cl-defun bisq-api-public-get-depth (market &key (result-type 'elisp)
                                            jsonpretty sync onsuccess params)
  "Provides list of open offer prices for a single market.
Calls the Bisq API depth action. Requires a valid Bisq
MARKET identifier.

Argument keywords, are optional, default values are used if
missing. Argument keywords are:

RESULT-TYPE: set to 'string to obtain the result as a json string,
             otherwise returned json is parsed to elisp with `json-read'.
JSONPRETTY: non-nil for pretty formatted json, defaul `nil'.
SYNC: set `t' for synchronous request. Default is `nil' (async).
ONSUCCESS: user supplied function to be called on success, for
           async requests only. Ignored when running synchronous
           requests.
PARAMS: initializes an empty alist, for collecting the API request
        parameters. Not for use by users.

Depending on the request options, the returned request data is a
json string or is parsed to elisp-types (default).

In async mode (default) will:
 1. save the `request-response' object to
    `bisq-api-last-request-response'. Regular request-response-*
    accessors can be applied to this object to obtain other
    information. See the documentation on `request.el'.
 2. save the returned request data in `bisq-api-public--data' to
    the cons cell with the first element named for the API action.

In synchronous mode (sync `t') will wait for the action to
complete and return the request result data, which are not
saved to `bisq-api-public--data'. `request-response' object
is saved to `bisq-api-last-request-response'."
  (unless jsonpretty (push (cons "format" "json") params))
  (push (cons "market" market) params)
  (setq bisq-api-public-last-request-response
        (bisq-api-public--request
          (concat bisq-api-public-url "depth/")
          :params params
          :result-type result-type
          :sync sync
          :success (cl-function
                    (lambda (&key data response &allow-other-keys)
                      (setf (alist-get (bisq-api-public-request-params-value response 'market)
                                       (alist-get 'depth bisq-api-public--data) nil nil #'string=)
                            (if (eq (bisq-api-public-request-settings response :parser) #'json-read)
                                (cdar data)
                              data))
                      (when (functionp onsuccess)
                        (funcall onsuccess :data data :response response))))))
  (when sync (request-response-data bisq-api-public-last-request-response)))


(cl-defun bisq-api-public-get-hloc (market &key interval
                                           timestamp_from
                                           timestamp_to
                                           jsonpretty
                                           (result-type 'elisp)
                                           sync onsuccess params)
  "Provides hi/low/open/close data for a given market.
This can be used to generate a candlestick chart. Calls the Bisq
API hloc action. Requires a valid Bisq MARKET identifier.

Argument keywords, are optional, default values are used if
missing. Argument keywords are:

INTERVAL: length of time blocks to summarize. auto will pick
          appropriate interval based on total time range.
          Valid, one of: minute, half_hour, hour, 
          half_day, day, week, month, year, auto (default).
TIMESTAMP_FROM: start time, in seconds since 1970. Default 2016-01-01.
TIMESTAMP_TO  : start time, in seconds since 1970. Default now.
RESULT-TYPE: set to 'string for result as string, otherwise is parsed
             to elisp with `json-read'. Default 'elisp.
JSONPRETTY: non-nil for pretty formatted json, default `nil'.
SYNC: set `t' for synchronous request. Default is `nil' (async).
ONSUCCESS: user supplied function to be called on success, for
           async requests only. Ignored when running synchronous
           requests.
PARAMS: initializes an empty alist, for collecting the API request
        parameters. Not for use by users.

Depending on the request options, the returned request data is a
json string or is parsed to elisp-types (default).

In async mode (default) will:
 1. save the `request-response' object to
    `bisq-api-last-request-response'. Regular request-response-*
    accessors can be applied to this object to obtain other
    information. See the documentation on `request.el'.
 2. save the returned request data in `bisq-api-public--data' to
    the cons cell with the first element named for the API action.

In synchronous mode (sync `t') will wait for the action to
complete and return the request result data, which are not
saved to `bisq-api-public--data'. `request-response' object
is saved to `bisq-api-last-request-response'. "
  (if (string= "csv" result-type)
      (push (cons "format" "csv") params)
    (unless jsonpretty (push (cons "format" "json") params)))
  (when timestamp_from (push (cons "timestamp_from" timestamp_from) params))
  (when timestamp_to   (push (cons "timestamp_to"   timestamp_to  ) params))
  (when (cl-find interval
                 '(minute half_hour hour half_day day week month year)
                 :test #'string=)
    (push (cons "interval" interval) params))
  (push (cons "market" market) params)
  (setq bisq-api-public-last-request-response
        (bisq-api-public--request
         (concat bisq-api-public-url "hloc/")
         :params params
         :result-type result-type
         :sync sync
         :succes (cl-function
                  (lambda (&key data response &allow-other-keys)
                    ;; save result to bap--data
                    (setf
                     (alist-get
                      (bisq-api-public-request-params-value response 'market)
                      (alist-get 'hloc bisq-api-public--data) nil nil #'string=)
                     (if (eq (bisq-api-public-request-settings response :parser)
                             #'json-read)
                         (cdar data)
                       data))
                    (when (functionp onsuccess)
                      (funcall onsuccess :data data :response response ))))))
  (when sync (request-response-data bisq-api-public-last-request-response)))

(cl-defun bisq-api-public-get-markets (&key (result-type 'elisp)
                                            jsonpretty
                                            sync onsuccess params)
  "Provides list of available markets.
Calls the Bisq API markets action.

Argument keywords, are optional, default values are used if
missing. Argument keywords are:

RESULT-TYPE: set to 'string or `t' for result as string,
             otherwise is parsed to elisp with `json-read'.
             Default 'elisp.
JSONPRETTY: non-nil for pretty formatted json, default `nil'.
SYNC: set `t' for synchronous request. Default is `nil' (async).
ONSUCCESS: user supplied function to be called on success, for
           async requests only. Ignored when running synchronous
           requests.
PARAMS: initializes an empty alist, for collecting the API request
        parameters. Not for use by users.

Depending on the request options, the returned request data is a
json string or is parsed to elisp-types (default).

In async mode (default) will:
 1. save the `request-response' object to
    `bisq-api-last-request-response'. Regular request-response-*
    accessors can be applied to this object to obtain other
    information. See the documentation on `request.el'.
 2. save the returned request data in `bisq-api-public--data' to
    the cons cell with the first element named for the API action.

In synchronous mode (sync `t') will wait for the action to
complete and return the request result data, which are not
saved to `bisq-api-public--data'. `request-response' object
is saved to `bisq-api-last-request-response'. "
  (unless jsonpretty (push (cons "format" "json") params))
  (setq bisq-api-public-last-request-response
        (bisq-api-public--request
         (concat bisq-api-public-url "markets/")
         :params params
         :result-type result-type
         :sync sync
         :succes (cl-function
                  (lambda (&key data response &allow-other-keys)
                    (setf (alist-get 'markets bisq-api-public--data) data)
                    (when (functionp onsuccess)
                      (funcall onsuccess :data data :response response))))))
  (when sync (request-response-data bisq-api-public-last-request-response)))

(cl-defun bisq-api-public-get-offers (market &key direction
                                             (result-type 'elisp)
                                             jsonpretty
                                             sync onsuccess params)
  "Provides list of open offer details for a single MARKET.
Calls the Bisq API offers action. MARKET is a valid Bisq market
identifier.

Argument keywords, are optional, default values are used if
missing. Argument keywords are:

DIRECTION: offer direction, values BUY, SELL. Omit for both.
RESULT-TYPE: set to 'string for result as string, otherwise is parsed
             to elisp with `json-read'. Default 'elisp.
JSONPRETTY: non-nil for pretty formatted json, default `nil'.
SYNC: set `t' for synchronous request. Default is `nil' (async).
ONSUCCESS: user supplied function to be called on success, for
           async requests only. Ignored when running synchronous
           requests.
PARAMS: initializes an empty alist, for collecting the API request
        parameters. Not for use by users.

Depending on the request options, the returned request data is a
json string or is parsed to elisp-types (default).

In async mode (default) will:
 1. save the `request-response' object to
    `bisq-api-last-request-response'. Regular request-response-*
    accessors can be applied to this object to obtain other
    information. See the documentation on `request.el'.
 2. save the returned request data in `bisq-api-public--data' to
    the cons cell with the first element named for the API action.

In synchronous mode (sync `t') will wait for the action to
complete and return the request result data, which are not
saved to `bisq-api-public--data'. `request-response' object
is saved to `bisq-api-last-request-response'. "
  (setq bisq-api-public-last-request-response
        (unless jsonpretty (push (cons "format" "json") params))
        (when (cl-find direction '("buy" "sell") :test #'string=)
          (push (cons "direction" direction) params))
        (push (cons "market" market) params)
        (bisq-api-public--request
          (concat bisq-api-public-url "offers/")
          :params params
          :result-type result-type
          :sync sync
          :success (cl-function
                    (lambda (&key data response &allow-other-keys)
                      (setf (alist-get (bisq-api-public-request-params-value response 'market)
                                       (alist-get 'offers bisq-api-public--data) nil nil #'string=)
                            (if (eq (bisq-api-public-request-settings response :parser) #'json-read)
                                (cdar data)
                              data))
                      (when (functionp onsuccess)
                        (funcall onsuccess :data data :response response))))))
  (when sync (request-response-data bisq-api-public-last-request-response)))

(cl-defun bisq-api-public-get-ticker (market &key (result-type 'elisp)
                                             jsonpretty
                                             sync onsuccess params)
  "Provides 24 hour price ticker for single market or all markets.
Calls the Bisq API ticker action. Argument MARKET is a valid
Bisq market identifier or `nil' for all markets.

Argument keywords, are optional, default values are used if
missing. Argument keywords are:

RESULT-TYPE: set to 'string for result as string, otherwise is parsed
             to elisp with `json-read'. Default 'elisp.
JSONPRETTY: non-nil for pretty formatted json, default `nil'.
SYNC: set `t' for synchronous request. Default is `nil' (async).
ONSUCCESS: user supplied function to be called on success, for
           async requests only. Ignored when running synchronous
           requests.
PARAMS: initializes an empty alist, for collecting the API request
        parameters. Not for use by users.

Depending on the request options, the returned request data is a
json string or is parsed to elisp-types (default).

In async mode (default) will:
 1. save the `request-response' object to
    `bisq-api-last-request-response'. Regular request-response-*
    accessors can be applied to this object to obtain other
    information. See the documentation on `request.el'.
 2. save the returned request data in `bisq-api-public--data' to
    the cons cell with the first element named for the API action.

In synchronous mode (sync `t') will wait for the action to
complete and return the request result data, which are not
saved to `bisq-api-public--data'. `request-response' object
is saved to `bisq-api-last-request-response'. "
  (setq bisq-api-public-last-request-response
        (unless jsonpretty (push (cons "format" "json") params))
        (when market (push (cons "market" market)))
        (bisq-api-public--request
         (concat bisq-api-public-url "ticker/")
         :params params
         :result-type result-type
         :sync sync
         :success (cl-function
                   (lambda (&key data response &allow-other-keys)
                     (setf (alist-get (bisq-api-public-request-params-value response 'market)
                                      (alist-get 'ticker bisq-api-public--data) nil nil #'string=)
                           (if (eq (bisq-api-public-request-settings response :parser) #'json-read)
                               (cdar data)
                             data))
                     (when (functionp onsuccess)
                       (funcall onsuccess :data data :response response))))))
  (when sync (request-response-data bisq-api-public-last-request-response)))

(cl-defun bisq-api-public-get-trades (market &key direction
                                             trade_id_from
                                             trade_id_to
                                             timestamp_from
                                             timestamp_to
                                             limit
                                             sort
                                             jsonpretty
                                             (result-type 'elisp)
                                             sync onsuccess params)
  "Provides list of completed trades for a single market.
Calls the Bisq API tickers action. Argument MARKET is a valid
Bisq market identifier or all for all markets.

Argument keywords, are optional, default values are used if
missing. Argument keywords are:

DIRECTION: offer direction, values BUY, SELL. Omit for both.
TRADE_ID_FROM: identifies first trade to include.
TRADE_ID_TO  : identifies last trade to include.
TIMESTAMP_FROM: start time, in seconds since 1970. Default 2016-01-01.
TIMESTAMP_TO  : start time, in seconds since 1970. Default now.
LIMIT: maximum trades to return. Max is 2000, default 100.
SORT: Sort by date, values asc, desc (default desc).
RESULT-TYPE: set to 'string for result as string, otherwise is parsed
             to elisp with `json-read'. Default 'elisp.
JSONPRETTY: non-nil for pretty formatted json, default `nil'.
SYNC: set `t' for synchronous request. Default is `nil' (async).
ONSUCCESS: user supplied function to be called on success, for
           async requests only. Ignored when running synchronous
           requests.
PARAMS: initializes an empty alist, for collecting the API request
        parameters. Not for use by users.

Depending on the request options, the returned request data is a
json string or is parsed to elisp-types (default).

In async mode (default) will:
 1. save the `request-response' object to
    `bisq-api-last-request-response'. Regular request-response-*
    accessors can be applied to this object to obtain other
    information. See the documentation on `request.el'.
 2. save the returned request data in `bisq-api-public--data' to
    the cons cell with the first element named for the API action.

In synchronous mode (sync `t') will wait for the action to
complete and return the request result data, which are not
saved to `bisq-api-public--data'. `request-response' object
is saved to `bisq-api-last-request-response'. "
  (unless jsonpretty (push (cons "format" "json") params))
  (when timestamp_from (push (cons "timestamp_from" timestamp_from) params))
  (when timestamp_to   (push (cons "timestamp_to"   timestamp_to  ) params))
  (when trade_id_from (push (cons "trade_id_from" trade_id_from) params))
  (when trade_id_to   (push (cons "trade_id_to"   trade_id_to  ) params))
  (when (cl-find direction '("buy" "sell") :test #'string=)
    (push (cons "direction" direction) params))
  (when (string= sort "asc") (push (cons "sort" 'asc) params))
  (when limit (push (cons "limit" limit) params))
  (push (cons "market" market) params)
  (setq bisq-api-public-last-request-response
        (bisq-api-public--request
         (concat bisq-api-public-url "trades/")
         :params params
         :result-type result-type
         :sync sync
         :succes (cl-function
                  (lambda (&key data response &allow-other-keys)
                    ;; save result to bap--data
                    (setf
                     (alist-get
                      (bisq-api-public-request-params-value response 'market)
                      (alist-get 'trades bisq-api-public--data) nil nil #'string=)
                     (if (eq (bisq-api-public-request-settings response :parser)
                             #'json-read)
                         (cdar data)
                       data))
                    (when (functionp onsuccess)
                      (funcall onsuccess :data data :response response ))))))
  (when sync (request-response-data bisq-api-public-last-request-response)))

(cl-defun bisq-api-public-get-volumes (&optional market basecurrency
                                              &key (format "json")
                                              (result-type 'elisp)
                                              interval
                                              jsonpretty
                                              sync onsuccess params)
  "Provides periodic volume data in terms of base currency for MARKET.
Market is required, here. The Bisq API differs, it allows to omit
the market identifier to return ticker data for all markets.
BASECURRENCY: As of 2017-07-24, supported basecurrencies are BTC, DOGE, LTC, DASH.
INTERVAL: length of time blocks to summarize. auto will pick appropriate interval
          based on total time range. Values, one of:
          minute, half_hour, hour, half_day, day, week, month, year, auto (default).
TIMESTAMP_FROM: start time, in seconds since 1970. Default 2016-01-01.
TIMESTAMP_TO  : start time, in seconds since 1970. Default now.

Either basecurrency or market param must be specified."
  ;; enforce consistency
  (cl-assert (xor market basecurrency) nil
             "Error: either basecurrency or market must be specified")
  (cl-assert (if basecurrency
                 (cl-find basecurrency '(BTC, DOGE, LTC, DASH) :test #'string=)
               t)
             nil
             "Error: supported basecurrencies for 'volume' api are BTC, DOGE, LTC, DASH")
  (unless (xor market basecurrency)
    (error "%s" "Error: either basecurrency or market must be specified"))
  (unless jsonpretty (push (cons "format" "json") params))
  (when timestamp_from (push (cons "timestamp_from" timestamp_from) params))
  (when timestamp_to   (push (cons "timestamp_to"   timestamp_to  ) params))
  (when (cl-find interval
                 '(minute half_hour hour half_day day week month year)
                 :test #'string=)
    (push (cons "interval" interval) params))
  (when basecurrency (push (cons "basecurrency" basecurrency) params))
  (when market (push (cons "market" market) params))
  (setq bisq-api-public-last-request-response
        (bisq-api-public--request
         (concat bisq-api-public-url "volumes/")
         :params params
         :result-type result-type
         :sync sync
         :succes (cl-function
                  (lambda (&key data response &allow-other-keys)
                    (setf (alist-get 'volumes bisq-api-public--data) data)
                    (when (functionp onsuccess)
                      (funcall onsuccess :data data :response response))))))
  (when sync (request-response-data bisq-api-public-last-request-response)))

(defun bisq-api-public-request-settings (response &optional keyvalue)
  "Get the request-repsonse RESPONSE settings for KEYVALUE.
Returns the complete request-response settings if keyvalue
is ommited."
  (if keyvalue
      (plist-get (request-response-settings response) keyvalue)
    (request-response-settings response)))

(defun bisq-api-public-request-params-value (response key)
  "Get the parameter value for KEY used in request RESPONSE."
  (alist-get key (bisq-api-public-request-settings response :params) nil nil #'string=))


;; (request-response-url bisq-api-last-request-response)
;; (assoc 'market
;;        (url-parse-query-string (second (split-string "https://markets.bisq.network/api/offers/?market=zec_btc&format=json" "?")))
;;        #'string=)
;; (plist-get dias-setting :params)
;; unix time: :time  (time-convert nil 'integer)

(provide 'bisq-api-public)
;;; bisq-api-public.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
