
# include <string.h>
# include "match.h"

# define MAX_CAPTURES   10

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
 * This function may be called by PCRE each time the (?C) pattern is
 * encountered, which we have arranged to occur at the end of each successful
 * pattern match. To find the rightmost match, we record the match found so
 * far if it is further right and/or longer than the last found match, then
 * tell PCRE to continue matching as if the match had failed.
 */
static
int rmatch_callout(pcre_callout_block *block)
{
  rmatch_data_t *rmatch = block->callout_data;

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
 * This function exists so that it can be called by the Haskell runtime
 * without any other threads running. This is necessary because it needs to
 * convey callout information to PCRE via global variable.
 */
int match_helper(const pcre *code, pcre_extra *extra,
                 const char *subject, int length,
                 int options, int ovector[MAX_CAPTURES * 3])
{
  extra->flags &= ~PCRE_EXTRA_CALLOUT_DATA;

  pcre_callout = 0;

  return pcre_exec(code, extra, subject, length, 0,
		   options, ovector, MAX_CAPTURES * 3);
}

/*
 * rmatch() helper function
 *
 * This function exists so that it can be called by the Haskell runtime
 * without any other threads running. This is necessary because it needs to
 * convey callout information to PCRE via global variable.
 */
int rmatch_helper(const pcre *code, pcre_extra *extra,
                  const char *subject, int length,
                  int options, int ovector[MAX_CAPTURES * 3])
{
  rmatch_data_t rmatch;
  int rc;

  rmatch.valid = 0;

  extra->callout_data = &rmatch;
  extra->flags |= PCRE_EXTRA_CALLOUT_DATA;

  pcre_callout = rmatch_callout;

  rc = pcre_exec(code, extra, subject, length, 0,
		 options, ovector, MAX_CAPTURES * 3);
  if (rc == PCRE_ERROR_NOMATCH && rmatch.valid) {
    rc = rmatch.valid;
    memcpy(ovector, &rmatch.ovec[0], sizeof(ovector[0]) * 2 * rc);
  }

  return rc;
}
