#include "cpio.h"

#include <stdlib.h>
#include <ctype.h>

#include "lib.h"

#define IN_SIZE		4096
#define OUT_SIZE	IN_SIZE * 4

enum State
{
	IDLE,
	NEED_NAME,
	NEED_LINK,
	MAKE_DIR,
	MAKE_LINK,
	WRITING_FILE,
	END
};

/* ReSharper disable CppDeclaratorNeverUsed */
typedef struct
{
	char	c_magic[6];		/* magic cookie */
	char	c_dev[6];		/* device number */
	char	c_ino[6];		/* inode number */
	char	c_mode[6];		/* file type/access */
	char	c_uid[6];		/* owners uid */
	char	c_gid[6];		/* owners gid */
	char	c_nlink[6];		/* # of links at archive creation */
	char	c_rdev[6];		/* block/char major/minor # */
	char	c_mtime[11];	/* modification time */
	char	c_namesize[6];	/* length of pathname */
	char	c_filesize[11];	/* length of file in bytes */
} Header;

static const char *MAGIC   = "070707";
static const char *TRAILER = "TRAILER!!!";

static CpioFile get_type(size_t mode);
static size_t parse_oct_num(const char *str, int len);

#ifdef _WIN32
void win_symlink(char *target, char *linkpath);
#endif

void cpio_init(Cpio *cpio, const char *shallowify)
{
	byte_buffer_init(&cpio->buffer, (size_t) OUT_SIZE);
	cpio->shallow = shallowify;
	cpio->dot_at = strlen(shallowify) - 1;
	cpio->state = IDLE;
	cpio->to_read = 0;
	cpio->file = (CpioHeader) { 0 };
}

void cpio_free(Cpio *cpio)
{
	byte_buffer_free(&cpio->buffer);
}

void cpio_push(Cpio *cpio, uint8_t *buffer, size_t len)
{
	if (cpio->state == END) return;
	size_t avail = 0;

	byte_buffer_write(&cpio->buffer, buffer, len);
	while ((avail = byte_buffer_available(cpio->buffer)) > 0)
	{
		switch ((enum State) cpio->state)
		{
			case IDLE:
				if (avail > sizeof(Header))
				{
					uint8_t hdr[sizeof(Header)];
					byte_buffer_read(&cpio->buffer, hdr, sizeof(Header));

					Header *header = (Header*) hdr;
					const size_t mode = parse_oct_num(header->c_mode, sizeof(header->c_mode));
					cpio->file.size = parse_oct_num(header->c_filesize, sizeof(header->c_filesize));
					const size_t namlen = parse_oct_num(header->c_namesize, sizeof(header->c_namesize));
					cpio->file.name = ccalloc(namlen + 1, sizeof(char));
					cpio->file.name_size = namlen;
					cpio->file.link = NULL;
					cpio->file.type = get_type(mode);
					if (cpio->file.type == SYMBOLIC_LINK)
					{
						cpio->file.link = ccalloc(cpio->file.size + 1, sizeof(char));
					}
					cpio->state = NEED_NAME;
					break;
				}
				return;
			case NEED_NAME:
				if (avail >= cpio->file.name_size)
				{
					byte_buffer_read(&cpio->buffer, (uint8_t*) cpio->file.name, cpio->file.name_size);

					switch (cpio->file.type)
					{
						case REGULAR:
							cpio->state = WRITING_FILE;
							break;
						case DIRECTORY:
							cpio->state = MAKE_DIR;
							break;
						case SYMBOLIC_LINK:
							cpio->state = NEED_LINK;
							break;
						case NONE:
							break;
					}

					if (str_eq(cpio->file.name, TRAILER))
					{
						cpio->state = END;
						break;
					}
					if (cpio->file.type == NONE)
					{
						error_exit("Cpio cannot continue");
					}
					break;
				}
				return;
			case NEED_LINK:
				if (avail >= cpio->file.size)
				{
					byte_buffer_read(&cpio->buffer, (uint8_t*) cpio->file.link, cpio->file.size);

					if (cpio->stage == SDK)
					{
						const char *start = strstr(cpio->file.name, "MacOSX");
						if (start)
						{
							start += 6;
							while (isdigit(*start)) start++;
							if (*start++ != '.') goto no_match;
							if (strcmp(start, "sdk") != 0) goto no_match;

							cpio->state = IDLE;
							break;
						}
					}
					else if (cpio->stage == SDK_INFO)
					{
						if (str_ends_with(cpio->file.name, "MacOSX.sdk"))
						{
							cpio->sdk = str_dup(cpio->file.link);
							cpio->state = IDLE;
							break;
						}
					}
no_match:
					cpio->state = MAKE_LINK;
					break;
				}
				return;
			case MAKE_DIR:
				if (str_start_with(cpio->file.name, cpio->shallow))
				{
					cpio->file.name[cpio->dot_at] = '.';
					dir_make(cpio->file.name + cpio->dot_at);
				}
				cpio->state = IDLE;
				break;
			case MAKE_LINK:
				if (str_start_with(cpio->file.name, cpio->shallow))
				{
					cpio->file.name[cpio->dot_at] = '.';
#ifdef _WIN32
					win_symlink(cpio->file.link, cpio->file.name +
						cpio->dot_at);
#else
					symlink(cpio->file.link, cpio->file.name +
						cpio->dot_at);
#endif
				}
				cpio->state = IDLE;
				break;
			case WRITING_FILE:
				break;
			case END:
				return;
		}

		if (cpio->state == WRITING_FILE)
		{
			uint8_t tmp[BUFSIZ];

			if (cpio->to_read == 0)
			{
				if (str_start_with(cpio->file.name, cpio->shallow))
				{
					cpio->file.name[cpio->dot_at] = '.';
					cpio->out = fopen(cpio->file.name + cpio->dot_at, "wb");
				}
				else
				{
					cpio->out = NULL;
				}
				cpio->to_read = cpio->file.size;
			}

			while (cpio->to_read && (avail = byte_buffer_available(cpio->buffer)) > 0)
			{
				const size_t left = MIN(cpio->to_read, BUFSIZ);
				const size_t read = byte_buffer_read(&cpio->buffer, tmp, left);
				if (cpio->out)
				{
					fwrite(tmp, read, sizeof(uint8_t), cpio->out);
				}
				cpio->to_read -= read;
			}

			if (cpio->to_read == 0)
			{
				if (cpio->out) fclose(cpio->out);
				cpio->state = IDLE;
			}
		}
	}
}

#define C_ISDIR 040000	 /* Directory */
#define C_ISREG 0100000 /* Regular file */
#define C_ISCTG 0110000 /* Reserved for contiguous files */
#define C_ISLNK 0120000 /* Reserved for symbolic links */
#define C_IFMT	0170000 /* type of file */

static CpioFile get_type(const size_t mode)
{
	const size_t masked = mode & C_IFMT;
	switch (masked)
	{
		case C_ISDIR:
			return DIRECTORY;
		case C_ISLNK:
			return SYMBOLIC_LINK;
		case C_ISCTG:
		case C_ISREG:
			return REGULAR;
		default:
			return NONE;
	}
}

static size_t parse_oct_num(const char *str, const int len)
{
	size_t temp_val = 0;
	const char *stop = str + len;

	while (str < stop && (*str == ' ' || *str == '0')) str++;
	while (str < stop && *str >= '0' && *str <= '7')
	{
		temp_val = (temp_val << 3) + (*str++ - '0');
	}
	return temp_val;
}