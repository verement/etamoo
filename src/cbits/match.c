
# include <string.h>
# include "match.h"

# define MAX_CAPTURES  10

/*
 * Structure for capturing rmatch results
 */
typedef struct {
  int ovec[MAX_CAPTURES * 2];
  int valid;
} rmatch_data_t;

/*
 * PCRE callout function used to capture rmatch results
 *
 * This function will be called by PCRE each time the (?C) pattern is
 * encountered, which we have arranged to occur at the end of each successful
 * pattern match. To find the rightmost match for rmatch, we record the match
 * found so far if it is further right and/or longer than the last found
 * match, then tell PCRE to continue matching as if the match had failed.
 *
 * This function is a no-op unless passed rmatch callout_data, allowing it to
 * be kept as the global PCRE callout function even for normal matches.
 */
static
int global_callout(pcre_callout_block *block)
{
  rmatch_data_t *rmatch = block->callout_data;

  if (!rmatch)
    return 0;  /* proceed as normal; not using rmatch */

  if (!rmatch->valid || block->start_match > rmatch->ovec[0] ||
      (block->start_match == rmatch->ovec[0] &&
       block->current_position > rmatch->ovec[1])) {
    /* make a copy of the offsets vector so the last such vector found can be
       returned as the rightmost match */
    rmatch->ovec[0] = block->start_match;
    rmatch->ovec[1] = block->current_position;
    memcpy(&rmatch->ovec[2], &block->offset_vector[2],
           sizeof(rmatch->ovec[2]) * 2 * (block->capture_top - 1));

    rmatch->valid = block->capture_top;
  }

  return 1;  /* cause match failure at current point, but continue trying */
}

/*
 * match() helper function
 *
 * Arrange to call pcre_exec() with a callout that does nothing. We use the
 * same callout function as rmatch_helper() so that we can run simultaneously
 * with it, as pcre_callout is a global variable.
 */
int match_helper(const pcre *code, const pcre_extra *extra,
                 const char *subject, int length,
                 int options, int ovector[MAX_CAPTURES * 3])
{
  /* we rely on the fact that *extra has no callout_data by default */

  pcre_callout = global_callout;

  return pcre_exec(code, extra, subject, length, 0,
		   options, ovector, MAX_CAPTURES * 3);
}

/*
 * rmatch() helper function
 *
 * Arrange to call pcre_exec() with a callout that records the rightmost
 * match. We use the same callout function as match_helper() so that we can
 * run simultaneously with it, as pcre_callout is a global variable.
 */
int rmatch_helper(const pcre *code, const pcre_extra *extra,
                  const char *subject, int length,
                  int options, int ovector[MAX_CAPTURES * 3])
{
  pcre_extra local_extra = *extra;
  rmatch_data_t rmatch;
  int rc;

  rmatch.valid = 0;

  /* modify a local copy to avoid interfering with any other threads */
  local_extra.callout_data = &rmatch;
  local_extra.flags |= PCRE_EXTRA_CALLOUT_DATA;

  pcre_callout = global_callout;

  rc = pcre_exec(code, &local_extra, subject, length, 0,
		 options, ovector, MAX_CAPTURES * 3);
  if (rc == PCRE_ERROR_NOMATCH && rmatch.valid) {
    rc = rmatch.valid;
    memcpy(ovector, &rmatch.ovec[0], sizeof(ovector[0]) * 2 * rc);
  }

  return rc;
}
