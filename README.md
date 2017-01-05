# terraform-hs

A haskell EDSL for generating [terraform][] infrastructure specifications.

The terraform system has some nice properties (an excellent workflow, support for many resource types over multiple cloud providers),
but as a programming language it has very limited data structures and abstraction cababilities. With this library
one can leverage haskell abstractions to specifying infrastructure, whilst relying on terraforms excellent features.

The terraform "API" is very broad, supporting hundreds of resource types accross multiple cloud providers. By it's nature
a haskell wrapping of this is involves significant boilerplate. This [boilerplate][] is [generated][] from an API specification.

Currently only a small subset of the AWS API is implemented, though
the addition of other resources and providers should be
straightforward.

The [examples][] directory illustrates API usage. 

[terraform]:https://www.terraform.io/
[boilerplate]:https://github.com/timbod7/terraform-hs/blob/master/src/Language/Terraform/Aws.hs
[generated]:https://github.com/timbod7/terraform-hs/blob/master/scripts/generate.hs
[examples]:https://github.com/timbod7/terraform-hs/blob/master/examples
