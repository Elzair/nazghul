#include <assert.h>
#include <ctype.h>
#include <stdlib.h>

static int dice_parse(const char *ptr, int *num, int *faces, int *bias)
{
        int state = 0;
        int val = 0;
        int sign = 1;

        *num = 0;
        *faces = 0;
        *bias = 0;

        while (*ptr) {
                switch (state) {

                case 0:
                        if (*ptr == '+') {
                                sign = 1;
                                state = 4;
                        } else if (*ptr == '-') {
                                sign = -1;
                                state = 4;
                        } else if (isdigit(*ptr)) {
                                val = *ptr - '0';
                                if (val)
                                        state = 1;
                        } else {
                                goto error;
                        }
                        break;
                        
                case 1:
                        if (*ptr == 'd') {
                                *num = val * sign;
                                val = 0;
                                sign = 1;
                                state = 2;
                        } else if (isdigit(*ptr)) {
                                val = (val * 10) + *ptr - '0';
                        } else {
                                goto error;
                        }
                        break;

                case 2:
                        if (isdigit(*ptr)) {
                                val = *ptr - '0';
                                if (val)
                                        state = 3;
                        } else {
                                goto error;
                        }
                        break;

                case 3:
                        if (isdigit(*ptr)) {
                                val = (val * 10) + *ptr - '0';
                        } else if (*ptr == '+') {
                                *faces = val;
                                val = 0;
                                state = 4;
                                sign = 1;
                        } else if (*ptr == '-') {
                                *faces = val;
                                val = 0;
                                state = 4;
                                sign = -1;
                        } else {
                                goto error;
                        }
                        break;

                case 4:
                        if (isdigit(*ptr)) {
                                val = atoi(ptr);
                                if (val)
                                        state = 5;
                        } else {
                                goto error;
                        }
                        break;

                case 5:
                        if (isdigit(*ptr)) {
                                val = (val * 10) + *ptr - '0';
                        } else {
                                goto error;
                        }
                        break;
                default:
                        assert(false); /* impossible state */
                        goto error;
                }

                ptr++;
        }

        switch (state) {
        case 0:
                break;
        case 1:
                *bias = val * sign;
                break;
        case 2:
                goto error;
                break;
        case 3:
                *faces = val * sign;
                break;
        case 4:
                *bias = val * sign;
                break;
        case 5:
                *bias = val * sign;
                break;
        default:
                assert(false); /* impossible state */
                goto error;
        }

        return 0;

 error:
        return -1;
}

int dice_roll_numeric(int num, int faces, int bias)
{
	int val = 0;
	while (num--) {
	       val += (rand() % faces) + 1;
	}
	val += bias;
	return val;
}

int dice_roll(const char *ptr)
{
        int num = 0;
        int faces = 0;
        int bias = 0;

        if (dice_parse(ptr, &num, &faces, &bias)) {
                assert(false); /* if uncertain, caller should have checked
                                * first */
                return 0;
        }
        
        return dice_roll_numeric(num,faces,bias);
}

int dice_valid(const char *fmt)
{
        int num, faces, bias;
        if (!fmt) 
                return 0;
        return dice_parse(fmt, &num, &faces, &bias) ? 0 : 1;
}

int dice_average(const char *fmt)
{
        int num, faces, bias;
        dice_parse(fmt, &num, &faces, &bias);
        return ((faces / 2) + 1) * num + bias;
}
