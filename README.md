This library is meant to be used in reflex-based applications that need to
dynamically allocate scarce, external resources and promptly free them when they're not
needed anymore. For this purpose, it provides a `ResourceT` monad with switching
capabilities that schedules deallocations and prevents improper referencing
of resources outside of their temporal scope (that is, after an event that caused
their deallocation has been fired).
