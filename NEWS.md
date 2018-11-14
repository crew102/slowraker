# slowraker 0.2.0

#### Misc changes

* `slowraker()` now provides allows the user to provide the phrase delimiters that will be used for splitting words into keywords via the `phrase_delims` parameter
* Square brackets are now treated as phrase delimiters by default
* Hyphens now get dropped between hyphenated words

# slowraker 0.1.1

#### Minor changes

* `slowrake()` refactored to increase speed (now ~ 4X faster than V0.1.0)

# slowraker 0.1.0

#### New functions

* `slowrake()` added to extract keywords from text using the RAKE algorithm
* `rbind_rakelist()` added to bind the list of data frames returned by `slowrake()` into a single data frame