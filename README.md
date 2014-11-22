cgroups-webapp
==============

Web app for manipulating cgroups in nixos

Written in Haskell with Happstack and Blaze

Needs root

Shows a horizontal nav bar of susbsytems followed by a three-column table: parent and children, pids in the group, pids not in the group which you can add by clicking them

Reads cgroup mount points from /proc/mounts including systemd which is wierd.

Read a group with HTTP GET at /subsystem/subgroup e.g. /memory gets the top level group in the memory subsystem.

PUT /subsystem/newgroup with blank body to create a group within the top level of the subsystem or /subsystem/existinggroup/newgroup 

PUT /subsystem/existinggroup with a numeric pid in the body to move that pid to that group.

POST, HEAD, etc do nothing.

Doesn't sanity check the body yet

Tries to avoid filesystem errors but doesn't catch them yet - you just get 500 if e.g. you're not root.
