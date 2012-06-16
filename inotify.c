#include <libguile.h>
#include <sys/inotify.h>
#include <unistd.h>
#include <stdlib.h>
#include <linux/limits.h>
#include <sys/types.h>
#include <sys/stat.h>

#define sti(X)     scm_to_int(X)
#define sfls(X)    scm_from_locale_symbol(X)
#define sfi(X)     scm_from_int(X)
#define sfui32(X)  scm_from_uint32(X)
#define cons(X, Y) scm_cons(X, Y)

SCM scm_inotify_init () {
    return sfi(inotify_init());
}

SCM scm_inotify_init1 (SCM flags) {
    return sfi(inotify_init1(sti(flags)));
}

SCM scm_inotify_aw(SCM fd, SCM path, SCM mask) {
    char q[NAME_MAX+1];
    scm_to_locale_stringbuf(path, q, NAME_MAX+1);

    return sfi(inotify_add_watch(sti(fd), q , scm_to_uint32(mask)));
}

SCM scm_inotify_rw(SCM fd, SCM wd) {
    return sfi(inotify_rm_watch(sti(fd), sti(wd)));
}

SCM scm_inotify_read(SCM fd) {
    int s = sizeof(struct inotify_event) + NAME_MAX + 1;

    struct inotify_event *q = malloc(s);

    // TODO: use k to return a multiple-value return.
    int k = read(sti(fd), q, s);

    SCM f = scm_list_5(
    	cons(sfls("wd"),      sfi(q->wd))
    	,cons(sfls("mask"),   sfui32(q->mask))
    	,cons(sfls("cookie"), sfui32(q->cookie))
    	,cons(sfls("len"),    sfui32(q->len))
    	,cons(sfls("name"),   (q->len > 0) ? sfls(q->name) : SCM_BOOL_F));

    free(q);

    return scm_list_2(f, sfi(k));
}

SCM scm_inotify_close(SCM fd) {
    return sfi(close(sti(fd)));
}

void init_inotify_module (void* v) {
    scm_c_define_gsubr("inotify-init-wrap", 0, 0, 0, scm_inotify_init);
    scm_c_define_gsubr("inotify-init1-wrap", 1, 0, 0, scm_inotify_init1);
    scm_c_define_gsubr("inotify-aw-wrap", 3, 0, 0, scm_inotify_aw);
    scm_c_define_gsubr("inotify-rw-wrap", 2, 0, 0, scm_inotify_rw);
    scm_c_define_gsubr("inotify-read-wrap", 1, 0, 0, scm_inotify_read);
    scm_c_define_gsubr("inotify-close-wrap", 1, 0, 0, scm_inotify_close);

    // inotify init constants.
    scm_c_define("inotify-init1-available-flags",
		 scm_list_2(
		     cons(sfls("in-nonblock"), sfui32(IN_NONBLOCK))
		     ,cons(sfls("in-cloexec"), sfui32(IN_CLOEXEC))));

    // inotify event constants, see inotify(7) for details.
    scm_c_define("inotify-events",
		 scm_list_n(
		     cons(sfls("in-access"),         sfui32(IN_ACCESS))
		     ,cons(sfls("in-attrib"),        sfui32(IN_ATTRIB))
		     ,cons(sfls("in-close-write"),   sfui32(IN_CLOSE_WRITE))
		     ,cons(sfls("in-close-nowrite"), sfui32(IN_CLOSE_NOWRITE))
		     ,cons(sfls("in-create"),        sfui32(IN_CREATE))
		     ,cons(sfls("in-delete"),        sfui32(IN_DELETE))
		     ,cons(sfls("in-delete-self"),   sfui32(IN_DELETE_SELF))
		     ,cons(sfls("in-modify"),        sfui32(IN_MODIFY))
		     ,cons(sfls("in-move-self"),     sfui32(IN_MOVE_SELF))
		     ,cons(sfls("in-moved-from"),    sfui32(IN_MOVED_FROM))
		     ,cons(sfls("in-moved-to"),      sfui32(IN_MOVED_TO))
		     ,cons(sfls("in-open"),          sfui32(IN_OPEN))
		     ,SCM_UNDEFINED));

    scm_c_define("inotify-add-watch-available-flags",
    		 scm_append(
    		     scm_list_2(
			 scm_variable_ref(
			     scm_c_lookup("inotify-events")),
    		     	 scm_list_n(
    		     	     cons(sfls("in-close"),        sfui32(IN_CLOSE))
    		     	     ,cons(sfls("in-move"),        sfui32(IN_MOVE))
    		     	     ,cons(sfls("in-dont-follow"), sfui32(IN_DONT_FOLLOW))
    		     	     ,cons(sfls("in-mask-add"),    sfui32(IN_MASK_ADD))
    		     	     ,cons(sfls("in-oneshot"),     sfui32(IN_ONESHOT))
    		     	     ,cons(sfls("in-onlydir"),     sfui32(IN_ONLYDIR))
    		     	     ,SCM_UNDEFINED))));

    scm_c_define("inotify-read-additional-masks",
        scm_list_n(
    	    cons(sfls("in-ignored"),     sfui32(IN_IGNORED))
    	    ,cons(sfls("in-isdir"),      sfui32(IN_ISDIR))
    	    ,cons(sfls("in-q-overflow"), sfui32(IN_Q_OVERFLOW))
    	    ,cons(sfls("in-unmount"),    sfui32(IN_UNMOUNT))
    	    ,SCM_UNDEFINED));

    scm_c_export(
	// functions
	"inotify-init-wrap",
	"inotify-init1-wrap",
	"inotify-aw-wrap",
	"inotify-rw-wrap",
	"inotify-read-wrap",
	"inotify-close-wrap",

	// constants
	"inotify-init1-available-flags",
	"inotify-add-watch-available-flags",
	"inotify-events",
	"inotify-read-additional-masks",
	NULL);
}

void scm_init_linux_inotify_cwrapper_module () {
    scm_c_define_module("linux inotify cwrapper", init_inotify_module, NULL);
}
