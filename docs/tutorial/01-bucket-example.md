# A First Example - hello-bucket

## Before you start

> Make sure you have [installed and configured](installation.md) `marv`.
> Also ensure that you have created an `alias` to invoke `marv`:
>     
>     alias marv="racket command.rkt"

## Let's start!

We are going to use "The Ubiquitous Bucket Example" - a recipe that will create
a storage bucket in a GCP project. After we've deployed it, we'll walk through
the code.

> ## Note
> You will need a GCP project that you can create your resources in.

## The Code
Start by copy/pasting this code into a new file `hello-world.mrv`:
```
#lang marv

import types/gcp/storage

module main() {
	hello-bucket = gcp:storage.bucket {
		project = env("MARV_GCP_PROJECT")
		region = env("MARV_GCP_REGION")
		name = imm: strf("~a-hello-world" project)
	}
}
```

## Deploying to GCP
 Run the following commands:

    export MARV_GCP_PROJECT=<your-gcp-project>
    export MARV_GCP_REGION=europe-west1  # change to suit your location
    export GCP_ACCESS_TOKEN=`gcloud auth print-access-token`
    
    marv --plan hello-world.mrv

Marv should report that it will create the bucket and you can go ahead and deploy it like this:

    marv --apply hello-world.mrv

Your new bucket is now ready for action! You will also notice that marv has created a **state file** in your current directory - this file is important as it holds details on the resources that have been created, and what attributes were configured. There's more about the state file in a later section.

## Break it down...

The first line of a marv file  tells the Racket interpreter to invoke Marv's DSL:

    #lang marv

Next we need to import the types for the GCP storage API - the types have to be imported (or declared) before they can be used by a module:

    import types/gcp/storage


> ## Note
> The type system is a a central part of what makes marv different from other IAC tools, and there will be much more to say about this in a later section.

All resources must be declared inside a module, and there must be a `main` module declared in the top-level file passed on the command line:

    module main() {...}


Now we come to our bucket’s resource declaration,  in which `hello-bucket`  is a `storage.bucket` resource that is managed by the `GCP` driver:

	hello-bucket = gcp:storage.bucket ...

> ## Note
> A resource declaration is generally of the form:
> 
>   **name** = **driver**:**type** **config-object**
>   
> A `driver` targets a specific cloud-provider's API; marv currently only supports `dev` and `gcp` (Google Cloud Platform).  `type` is the kind of resource supported by the cloud provider, such as `compute.instance` or `secret-manager.secret`.

Finally, we have the body of our resource declaration:

```
{
	project = env("MARV_GCP_PROJECT")
	region = env("MARV_GCP_REGION")
	name = imm: strf("~a-hello-world" project)
}
```

This is a `config-object` which is basically a set of `name=value` attributes, enclosed in `{  }` . `project` and `region` attributes are assigned from the named environment variables. 

`name` is assigned the results of the `strf` function which replaces `~a` by the value of `project` (which was read from the environment). So if `MARV_GCP_PROJECT` is 'my-project' then the bucket's name is `my-project-hello-world`.

`imm:` is a marker to indicate to marv that the `name` attribute is **immutable** -  this means if it changes then the bucket must be recreated.

> ## Note
> We are working on auto-discovering which fields are immutable so (hopefully) you won't need to do as much of this manual tagging in a future release.

# Next steps

That completes our first steps with `marv` - in the next section we'll cover more of the language features. However, if you want to stop and delete all the resources for now you can run the purge command:

    marv --purge --apply hello-world.mrv

# MORE TUTORIALS COMING SOON

In the meantime, you can look at the [examples](examples) for more information.