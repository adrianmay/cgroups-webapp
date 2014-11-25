cgroups-webapp
==============

Web app for manipulating cgroups in nixos

Written in Haskell with Happstack and Blaze

Listens on 8000 (configurable) but still needs root to write to the cgroup vfs.

Shows the current group name followed by a horizontal nav bar of subsystems followed by a three-column table: links to parent and children, pids in the group, pids not in the group which you can add to it by clicking them. There's also a button to make a new group as a child of the current one which prompts for the name. You can click on anything blue.

Reads cgroup mount points from /proc/mounts including systemd which is wierd. Mounts don't need a common root - they can be all over the place.

Read a group with HTTP GET at /subsystem/subgroup e.g. /memory gets the top level group in the memory subsystem.

PUT /subsystem/newgroup with blank body to create a group within the top level of the subsystem or /subsystem/existinggroup/newgroup 

PUT /subsystem/existinggroup with a numeric pid in the body to move that pid to that group. Some pids don't like to move, but I don't know why not.

POST, HEAD, etc do nothing.

TODO: CSS.


