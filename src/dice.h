
/* A dice roll is specified by a string format. For example, "2d20+6". The
 * legal formats are defined by a regular expression:
 *
 *      ([1-9][0-9]*]d[1-9][0-9]*)?([\+\-]?[1-9][0-9]*)?
 *
 * Which means the following are all legal examples:
 *
 *      "1d1"
 *      "2"
 *      "" (note: returns 0)
 *      "5064d21023902-10909012"
 *
 * The result of the dice roll is returned. If the format is invalid then it
 * will always return 0, but since 0 is a valid response you won't necessarily
 * be able to tell. To check if a format is bad use the separate dice_valid()
 * call, which returns non-zero if the format is ok and 0 otherwise.
 *
 */
extern int dice_roll(const char *fmt);
extern int dice_valid(const char *fmt);
extern int dice_average(const char *fmt);
extern int dice_roll_numeric(int num, int faces, int bias);
