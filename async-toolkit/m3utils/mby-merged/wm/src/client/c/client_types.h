/* In the future, the content of this message could be auto-generated */

#define MODEL_MSG_HEADER_SIZE	12
#define MODEL_VERSION			2
#define MODEL_MSG_LINK_STATE	1
#define MODEL_MSG_ERROR			10
#define MODEL_MSG_IOSF			11
#define MODEL_MSG_CTRL			12
#define MODEL_MSG_VERSION_INFO	13
#define MODEL_MSG_NVM_READ		14
#define MODEL_MSG_COMMAND_QUIT	15

/* Max length of IOSF messages used for read/write operations */
#define IOSF_MSG_MAX_LEN		512

/* In HLP this is the max length of a mailbox message */
#define MAX_MSG_LEN 4096

struct wm_cq_msg {
	/* -- 16/32-bit members must be aligned to a 2/4-byte boundary -- */

	/** Length of the entire message in bytes, including the header */
	uint32_t msg_length;

	/** The wm_cq_msg version. */
	uint16_t version;

	/** Message type (see IES_MODEL_MSG_*). */
	uint16_t type;

	/** Target model switch number for this message. */
	uint16_t sw;

	/** Target model switch port for this message. */
	uint16_t port;

	/* Data buffer.
	 *
	 * For SBIOSF messages:
	 * - Buffer with SAI header from server point of view
	 * - From API perpective, buffer does not include SAI header
	 */
	uint8_t data[MAX_MSG_LEN];
};

