#lang racket/base
(require json)

(provide patches)

; This patch adds a missing 'required: true' to the secretId

(define secret-create (string->jsexpr #<<EOF
{
  "id": "secretmanager.projects.secrets.create",
  "path": "v1/{+parent}/secrets",
  "flatPath": "v1/projects/{projectsId}/secrets",
  "httpMethod": "POST",
  "parameters": {
    "parent": {
      "description": "Required. The resource name of the project to associate with the Secret, in the format `projects/*`.",
      "pattern": "^projects/[^/]+$",
      "location": "path",
      "required": true,
      "type": "string"
    },
    "secretId": {
      "description": "Required. This must be unique within the project. A secret ID is a string with a maximum length of 255 characters and can contain uppercase and lowercase letters, numerals, and the hyphen (`-`) and underscore (`_`) characters.",
      "location": "query",
      "type": "string",
      "required": true
    }
  },
  "parameterOrder": [
    "parent"
  ],
  "request": {
    "$ref": "Secret"
  },
  "response": {
    "$ref": "Secret"
  },
  "scopes": [
    "https://www.googleapis.com/auth/cloud-platform"
  ],
  "description": "Creates a new Secret containing no SecretVersions."
}
EOF
                                      ))

(define patches (hash 'secretmanager.projects.secrets.create secret-create))