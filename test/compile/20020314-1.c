struct socket {
  int a;
};
struct tux_req_struct
{
        struct socket *sock;
        char usermode;
        char *userbuf;
        unsigned int userlen;
        char error;
        void *private;
};
typedef struct tux_req_struct tux_req_t;
void add_output_space_event(tux_req_t *req, struct socket * sock);
void del_tux_atom(tux_req_t *req);
void add_req_to_workqueue(tux_req_t *req);
void user_send_buffer (tux_req_t *req, int cachemiss)
{
        int ret;
repeat:
        switch (ret) {
                case -11:
                        if (add_output_space_event(req, req->sock)) {
                                del_tux_atom(req);
                                goto repeat;
                        }
                        do { } while (0);
                        break;
                default:
                        add_req_to_workqueue(req);
        }
}
