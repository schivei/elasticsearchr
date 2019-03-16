# Copyright 2016 Alex Ioannides
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


#' Simple Elasticsearch default mappings for non-text-search analytics
#'
#' This mapping switches-off the text analyser for all fields of type 'string' (i.e. switches off
#' free text search), allows all text search to work with case-insensitive lowercase terms, and
#' maps any field with the name 'timestamp' to type 'date', so long as it has the appropriate
#' string or long format.
#'
#' @export
mapping_default_simple <- function(url = NULL) {
  doc_start <- '"_default_": {'
  doc_end <- '}'

  if (!is.null(resource) && is_elastic_resource(resource)) {
    version <- elastic(url) %info% es_version(resource)

    if (version$major >= 7) {
      doc_start <- ''
      doc_end <- ''
    }
  }

  jsonlite::prettify(paste0(
  '{
    "settings": {
      "index": {
        "analysis": {
          "analyzer": {
            "analyzer_lowercase": {
              "tokenizer": "keyword",
              "filter": "lowercase"
            }
          }
        }
      }
    },
    "mappings": {
      ', doc_start, '
        "dynamic_templates": [
          {
            "strings": {
              "match_mapping_type": "string",
              "mapping": {
                "type": "string",
                "analyzer": "analyzer_lowercase"
              }
            }
          },
          {
            "time": {
              "match": "timestamp",
              "mapping": {
                "type": "date",
                "format": "yyyy-MM-dd HH:mm:ss.SSS||yyyy-MM-dd||epoch_millis"
              }
            }
          }]
      ', doc_end, '
    }
  }'))
}


#' Elasticsearch 5.x default mappings enabling fielddata for text fields
#'
#' A default mapping that enables fielddata for all string/text fields in Elasticsearch 5.x.
#'
#' @export
mapping_fielddata_true <- function() {
  doc_start <- '"_default_": {'
  doc_end <- '}'

  if (!is.null(resource) && is_elastic_resource(resource)) {
    version <- elastic(url) %info% es_version(resource)

    if (version$major >= 7) {
      doc_start <- ''
      doc_end <- ''
    }
  }

  jsonlite::prettify(paste0(
  '{
    "mappings": {
      ', doc_start, '
        "dynamic_templates": [
          {
            "strings": {
              "match_mapping_type": "string",
              "mapping": {
                "type": "text",
                "fielddata": true
              }
            }
          }
        ]
      ', doc_end, '
    }
}'))
}
