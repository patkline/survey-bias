# Sample Prep Row-Drop Report (by Claude AI)
Prompt: 1. Read sample_prep.R and the relevant helper function files carefully and tell me if long_survey_final.csv is restricted to those in the leave in connected set. 2. Figure out how many rows are dropped in each step and send me a report.

**Starting data:** 35,865 rows | 7,173 respondents | 165 firms

## Drops by step (per outcome)

| Step | Biggest dropper | Typical resp_id drop |
|---|---|---|
| **1. Drop NA outcome** | All bias outcomes lose ~19,500+ rows (~3,800 resp); `discretion`/`FirmSelective`/`FirmDesire` lose far fewer (~2,600-3,400 rows) | ~50% of respondents for bias items; ~3-6% for non-bias items |
| **2. Keep >2 firms per respondent** | Small across all outcomes | 37-152 resp_ids |
| **3. Drop no-variation respondents** (min == max rating) | Largest filter after NA. Drops 4,400-10,100 rows depending on outcome | 900-2,068 resp_ids |
| **4. Drop duplicate firm rankings** | Negligible | 0-2 resp_ids |
| **5. Leave-in-connected set** | **Zero rows dropped for every outcome** | 0 resp_ids; all 164 firms remain connected |

## Detailed per-outcome breakdown

### FirmCont_favor_white
- After drop NA outcome:        16,170 rows (-19,695) | 3,339 resp_ids (-3,834)
- After >2 firms per resp:      16,114 rows (-56)      | 3,302 resp_ids (-37)
- After drop no-variation:      10,500 rows (-5,614)   | 2,160 resp_ids (-1,142)
- After drop dup firm ranks:    10,500 rows (+0)       | 2,160 resp_ids (+0)
- After leave-in-connected:     10,500 rows (+0)       | 2,160 resp_ids (+0) | firms: 164 -> 164

### FirmHire_favor_white
- After drop NA outcome:        16,181 rows (-19,684) | 3,335 resp_ids (-3,838)
- After >2 firms per resp:      16,104 rows (-77)      | 3,292 resp_ids (-43)
- After drop no-variation:       9,817 rows (-6,287)   | 2,015 resp_ids (-1,277)
- After drop dup firm ranks:     9,817 rows (+0)       | 2,015 resp_ids (+0)
- After leave-in-connected:      9,817 rows (+0)       | 2,015 resp_ids (+0) | firms: 164 -> 164

### conduct_favor_white
- After drop NA outcome:        16,206 rows (-19,659) | 3,370 resp_ids (-3,803)
- After >2 firms per resp:      16,097 rows (-109)     | 3,296 resp_ids (-74)
- After drop no-variation:       7,912 rows (-8,185)   | 1,631 resp_ids (-1,665)
- After drop dup firm ranks:     7,907 rows (-5)       | 1,630 resp_ids (-1)
- After leave-in-connected:      7,907 rows (+0)       | 1,630 resp_ids (+0) | firms: 164 -> 164

### FirmCont_favor_male
- After drop NA outcome:        16,241 rows (-19,624) | 3,355 resp_ids (-3,818)
- After >2 firms per resp:      16,161 rows (-80)      | 3,304 resp_ids (-51)
- After drop no-variation:      11,734 rows (-4,427)   | 2,404 resp_ids (-900)
- After drop dup firm ranks:    11,734 rows (+0)       | 2,404 resp_ids (+0)
- After leave-in-connected:     11,734 rows (+0)       | 2,404 resp_ids (+0) | firms: 164 -> 164

### FirmHire_favor_male
- After drop NA outcome:        16,209 rows (-19,656) | 3,351 resp_ids (-3,822)
- After >2 firms per resp:      16,136 rows (-73)      | 3,304 resp_ids (-47)
- After drop no-variation:      11,333 rows (-4,803)   | 2,329 resp_ids (-975)
- After drop dup firm ranks:    11,333 rows (+0)       | 2,329 resp_ids (+0)
- After leave-in-connected:     11,333 rows (+0)       | 2,329 resp_ids (+0) | firms: 164 -> 164

### conduct_favor_male
- After drop NA outcome:        16,388 rows (-19,477) | 3,411 resp_ids (-3,762)
- After >2 firms per resp:      16,288 rows (-100)     | 3,347 resp_ids (-64)
- After drop no-variation:       9,898 rows (-6,390)   | 2,043 resp_ids (-1,304)
- After drop dup firm ranks:     9,894 rows (-4)       | 2,042 resp_ids (-1)
- After leave-in-connected:      9,894 rows (+0)       | 2,042 resp_ids (+0) | firms: 164 -> 164

### conduct_favor_younger
- After drop NA outcome:        15,673 rows (-20,192) | 3,273 resp_ids (-3,900)
- After >2 firms per resp:      15,559 rows (-114)     | 3,201 resp_ids (-72)
- After drop no-variation:      11,034 rows (-4,525)   | 2,277 resp_ids (-924)
- After drop dup firm ranks:    11,030 rows (-4)       | 2,276 resp_ids (-1)
- After leave-in-connected:     11,030 rows (+0)       | 2,276 resp_ids (+0) | firms: 164 -> 164

### discretion
- After drop NA outcome:        32,432 rows (-3,433)  | 6,720 resp_ids (-453)
- After >2 firms per resp:      32,255 rows (-177)     | 6,606 resp_ids (-114)
- After drop no-variation:      23,215 rows (-9,040)   | 4,757 resp_ids (-1,849)
- After drop dup firm ranks:    23,205 rows (-10)      | 4,755 resp_ids (-2)
- After leave-in-connected:     23,205 rows (+0)       | 4,755 resp_ids (+0) | firms: 164 -> 164

### FirmSelective
- After drop NA outcome:        33,110 rows (-2,755)  | 6,966 resp_ids (-207)
- After >2 firms per resp:      32,839 rows (-271)     | 6,814 resp_ids (-152)
- After drop no-variation:      27,374 rows (-5,465)   | 5,674 resp_ids (-1,140)
- After drop dup firm ranks:    27,366 rows (-8)       | 5,672 resp_ids (-2)
- After leave-in-connected:     27,366 rows (+0)       | 5,672 resp_ids (+0) | firms: 164 -> 164

### FirmDesire
- After drop NA outcome:        33,264 rows (-2,601)  | 6,907 resp_ids (-266)
- After >2 firms per resp:      33,067 rows (-197)     | 6,789 resp_ids (-118)
- After drop no-variation:      22,970 rows (-10,097)  | 4,721 resp_ids (-2,068)
- After drop dup firm ranks:    22,960 rows (-10)      | 4,719 resp_ids (-2)
- After leave-in-connected:     22,960 rows (+0)       | 4,719 resp_ids (+0) | firms: 164 -> 164

## Key takeaway on the connected set

**The leave-in-connected-set filter drops nothing.** All 164 firms (with non-missing names) form a single connected component for every outcome. This means the connected-set restriction is not actually binding in this data.

## Final union filter

After taking the union of surviving resp_ids across all 10 outcomes:
- **6,515** of 7,173 respondents survive (658 dropped, ~9.2%)
- **32,575** of 35,865 rows survive (3,290 dropped, ~9.2%)

The 658 dropped respondents are those who failed all 10 outcomes -- typically because they had missing data on every bias question AND gave identical ratings on `discretion`/`FirmSelective`/`FirmDesire`.

## The two steps that do the real work

1. **NA in outcome** -- drives the per-outcome sample sizes (especially for bias items where ~half the sample was not asked)
2. **No-variation filter** -- the second largest cut, removing respondents who gave every firm the same rating