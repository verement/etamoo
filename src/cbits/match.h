
# include <pcre.h>

int  match_helper(const pcre *, const pcre_extra *,
		  const char *, int, int, int *);
int rmatch_helper(const pcre *, const pcre_extra *,
		  const char *, int, int, int *);
