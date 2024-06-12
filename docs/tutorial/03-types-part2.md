# Part 2

So to recap, our module currently looks like this:

```
#lang marv

import types/gcp/storage as storage

type labelledBucket = {
    identity(cfg) = cfg <- { 
        labels = { costcentre = "abc123", environment="dev" } 
    }
    * = storage:bucket.*
}

defaults = {
    project = env("MARV_GCP_PROJECT") 
    region = env("MARV_GCP_REGION") 
}

module main {

    bucket1 = labelledBucket defaults <- {
        name = imm: strf("~a-hello-world1" defaults.project)
    }

    bucket2 = labelledBucket defaults <- {
        name = imm: strf("~a-hello-world2" defaults.project)
    }
    
}
```
