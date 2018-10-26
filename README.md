proper_example
=====

Used as an example system for property based testing. Contains six different tags:
- 1-init
  Simple working setup
- 2-table-state
  Check that tables in kv match our expectation
- 3-table-unique-names
  Unique sorting to fix test
- 4-post-condition
  Check that the create table result works
  It will fail on duplicate table names
- 5-existing-tables
  Explicitly check for the failing case
- put-items
  Test put operation


Other useful links:

  https://proper-testing.github.io/apidocs/
  https://propertesting.com/toc.html
