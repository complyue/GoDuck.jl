
> (Raw Machine) Performance put aside, only then you can go *really* **DYNAMIC**.

> **Julia** declares itself as a *dynamic* programming language, but in a sense not **DYNAMIC** enough: 

>> All variables have to be declared (explicitly or implicitly) before first use, as they need to be bound/enclosed **STATICALLY** per the time source code get Just-In-Time compiled.

>> Only module-global scopes are open, other (so as Julia calls them, local) scopes are closed by design. And guess what? Module-global scopes have no lexical access at all, despite there can be hierarchies of nested modules!

> That's understandable software-engineering-disciplines for computer based system implementations, but does obscure the business-wise semantical mindset for clear understanding and communication among users of the system. Many speaks about will address implementation details rather than business essentials.

> To facilitate a better foundation for DSLs (Domain Specific Languages), especially ones powering multi-user concurrent interactions, you'd often better provide a dynamic envionment (i.e. hierarchy of scopes), that directly maps to your business model.
