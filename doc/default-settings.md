
# Default Settings

## Default User Code

By default the Quartus compiler was setting the user code bits to
be the computed check sum. This added unwanted noise to the fuse
list when comparing two experiments.

Therefore a default user code is used for each compilation.
See `setting:default/1`.

A user code of `00000000` was chosen rathe than `FFFFFFFF`
since it produced less fuses.

## Default Seed

Another setting with a default is the *seed* with a default of 1.

Make the setting explicit in cached experiments to improve caching in the
future if / when this setting starts to get used.

