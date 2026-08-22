# tung documentation

this project buildeth the searchable standard-bookhoard html reference from shown
declarations, fills, and attached doc comments in the repository-level
`bookhoard/` tree. generated links connect modules, declaration owners and
members, signature references, shapes, fill targets, and their reverse usages.

```sh
npm install
npm test
npm run docs
```

the generated page is `bookhoard/index.html`. it is ignored and must not be
edited by hand.

the root `make setup` command installeth dependencies, runneth these checks and tests, and
generateth the page as part of the full development setup. `make docs` only
regenerateth the page.

the generator reuseth the tolerant source and visibility model under
`vscode/server/`; vscode doth not build or package the generator.
