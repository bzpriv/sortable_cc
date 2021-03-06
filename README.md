# Sortable Challenge

To run:

```bash
git clone git@github.com:bzpriv/sortable_cc
cd sortable_cc
sbt run
cat results.txt
```

### Assumptions
* The provided data set is representative of expected input over time. Tailoring a solution to the given data set will accomplish the desired goal.
* Listings that bundle accessories with a product should still match when possible
* Listings should be trusted as intentional, without attempting to correct for potential mistakes.
  * Product A should not match product AA even with a close edit distance between the two.
  * Product A with manufacturer B is different from product A with manufacturer C, and the two cannot be matched

### Basic Strategy

Based on the third assumption, we can scope comparisons between listings and products to manufacturer, which
will dramatically assist with runtime.

Initially we can do a naive comparison based on exact match
(if product substring of listing or listing substring of product) and then by examining missed matches we
can identify opportunities for adjusting the comparison to account for common discrepancies.

On reading products and listings out of their files, we normalize the data to support effective comparison.

After grouping the products by manufacturer, we can take a single pass over the listings, comparing them to
products with a matching manufacturer and grouping them into one of 5 buckets:

* No matching manufacturer
* Multiple matching manufacturers
* No matching products under single matching manufacturer
* Multiple matching products under single matching manufacturer
* Single matching product under single matching manufacturer

Only the final bucket will be kept for final results.

### Data Normalization and Match Strategy

After examining missed and duplicate matches on a first pass with naive implementation, the following
adjustments revealed themselves as high roi opportunities for increasing match rate:

* Mapping manufacturers to canonical names. Two manufacturers in particular missed non-trivial amounts of matches due to mismatched aliases. Mappings were added [here](https://github.com/bzpriv/sortable_cc/blob/master/src/main/scala/Solution.scala#L63)
* Removing manufacturer and family when present from product name. A significant number of false negatives resulted due to inconsistent inclusion of manufacturer and/or family in both product and listing names. This is done [here](https://github.com/bzpriv/sortable_cc/blob/master/src/main/scala/Solution.scala#L52")
  * Only one case of duplicate product name (after normalization) was found, (Samsung-SL202 and Samsung_SL202) and the records involved both had no family. This means that within a manufacturer, it would be impossible to match listing A with family B to product A with family C, so this type of false positive would not be a risk. It does leave room for matching listing A with family B to product A with no family, which is assumed sufficient here. If this was problematic, a check for family when comparing listings to products, in the same manner as name is used, would eliminate this source of false positives.
* Skipping the prior adjustment when it would result in using a common domain term as a product name. Particularly a product with model "zoom" was seen to create a significant number of false positives within it's manufacturer for the fourth match bucket. This is done [here](https://github.com/bzpriv/sortable_cc/blob/master/src/main/scala/Solution.scala#L56)
* Deduping products by normalized product name. Products were seen to contain both Samsung-SL202 and Samsung_SL202 which clearly point to the same product and match completely after normalization, creating false positives for the fourth match bucket for any listings that matched this product. This is done [here](https://github.com/bzpriv/sortable_cc/blob/master/src/main/scala/Solution.scala#L197)
* Various checks to account for inconsistent placement/presence of spaces and hyphens helped to marginally improve match rates. This is done [here](https://github.com/bzpriv/sortable_cc/blob/master/src/main/scala/Solution.scala#L207)

Price outliers were evaluated as a potentially effective indicator of false matches, but close examination of the
results suggested that this would disproportionately create false negatives with minimal reduction in false matches.
Among the 12 products with the widest spread of listing prices, only 1-3 matched listings per product appeared to be false matches, while valid matches were seen which had top prices up to 10x bottom prices.

### Potential next steps

* With products loaded into memory, listings could be streamed from input file to output file, dramatically reducing memory footprint.
* Products could be moved to central storage or duplicated and work can be distributed. Parallel processing of listings would be possible without further modification, barring a potential deduplication on loading distributed results to a common location.
* Manufacturer mappings and protected terms can be given administrative tooling. Job statistics can be used to report likely candidates for new mappings and terms as data is processed, and notifications made available for manual review/approval, allowing the system to get smarter with minimal manual intervention.

