#lang racket/base
(require json)

(provide patches)

; This patch adds a missing 'required: true' to the name field

(define user-delete (string->jsexpr #<<EOF

{
          "id": "sql.users.delete",
          "path": "v1/projects/{project}/instances/{instance}/users",
          "flatPath": "v1/projects/{project}/instances/{instance}/users",
          "httpMethod": "DELETE",
          "parameters": {
            "project": {
              "description": "Project ID of the project that contains the instance.",
              "location": "path",
              "required": true,
              "type": "string"
            },
            "instance": {
              "description": "Database instance ID. This does not include the project ID.",
              "location": "path",
              "required": true,
              "type": "string"
            },
            "host": {
              "description": "Host of the user in the instance.",
              "location": "query",
              "type": "string"
            },
            "name": {
              "description": "Name of the user in the instance.",
              "location": "query",
              "type": "string",
              "required": true
            }
          },
          "parameterOrder": [
            "project",
            "instance"
          ],
          "response": {
            "$ref": "Operation"
          },
          "scopes": [
            "https://www.googleapis.com/auth/cloud-platform",
            "https://www.googleapis.com/auth/sqlservice.admin"
          ],
          "description": "Deletes a user from a Cloud SQL instance."
        }
EOF
                                    ))

(define patches (hash 'sql.users.delete user-delete))