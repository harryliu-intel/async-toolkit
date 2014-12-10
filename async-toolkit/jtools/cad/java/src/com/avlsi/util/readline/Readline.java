/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

// vim:sw=4:expandtab:ai:cin:ts=4
package com.avlsi.util.readline;

import java.util.Map;
import java.util.HashMap;
import com.avlsi.util.debug.Debug;

/**
 * Class providing an interface to GNU readline and clones.
 *
 * Legalities:
 **/
public class Readline {
    static {
        System.loadLibrary("Readline");
        native_init();
    }

    /**
     * This class should not be instantiated.
     **/
    private Readline() { }

    // Function type declarations
    private static Function startup_hook = null;
    private static Function pre_input_hook = null;
    private static Function event_hook = null;

    public interface Function {
        int run();
    }

    /** Function returning void. **/
    public interface VFunction {
        void run();
    }

    /** Function returning pointer to char, (String) **/
    public interface CPFunction {
        String run();
    }

    /** Function returning pointer to pointer to char, (Pointer to String). **/
    public interface CPPFunction {
        String[] run();
    }

    /**
     * "Keyboard invoked function".  See the keymaps.  Key is the invoking key, count
     * is the somewhat arbitrary prefix argument.  Do note that you
     * may receive negative counts.  
     **/
    public interface KeyboardFunction {
        /**
         * @param key invoking key
         * @param count somewhat arbitrary prefix argument. 
         *              Do note that you may receive negative counts.
         **/
        int run(int count, int key);
    }

    /**
     * functions compatible with the "rl_completion_entry_function".
     * AKA generator functions.
     *
     * Sadly, their is no way to call this function saying "no more",
     * which would be nice for releasing resources.
     *
     * rl_attempted_completion_function is not yet wrapped.
     **/
    public interface CompletionFunction {
    /**
     * @param text partial word to be completed.
     * @param state 0 for first time function is called, non-zero thereafter.
     *              You can often get away with generating matches the
     *              first time, and then stepping through on subsequent
     *              calls.
     **/
        String run(String text, int state);
    }
    public interface AttemptCompletionFunction {
        /** @param text command line to be completed 
            @param start start of word @param end end of word **/
        String[] run(String text, int start, int end);
    }

    // Readline data structures
    public static class Keymap {
        private static final int SIZE = 256;

        /** Mapping between C Keymaps and Java Keymaps */
        private static final Map map = new HashMap();
        private final int address;
        private final Entry[] entries;
        /** Immutable Keymap Entry. */
        public static class Entry {
            private final KeyboardFunction function;
            private Entry(final KeyboardFunction function) {
                this.function = function;
            }
            public static Entry makeEntry(final KeyboardFunction function) {
                return new Entry(function);
            }
            public KeyboardFunction getFunction() {
                return function;
            }
        }
        private Keymap(final int address) {
            this.address = address;
            entries = new Entry[SIZE];
            map.put(new Integer(address), this);
        }
        public static Keymap makeKeymap(final int address) {
            return new Keymap(address);
        }
        public static void registerFunction(int address, int key,
                                            KeyboardFunction func) {
            Integer addr = new Integer(address);
            if (!map.containsKey(addr)) {
                new Keymap(address);
            }
            ((Keymap) map.get(addr)).entries[key] = Entry.makeEntry(func);
        }
        public static void unregisterFunction(int address, int key) {
            Integer addr = new Integer(address);
            if (map.containsKey(addr)) {
                ((Keymap) map.get(addr)).entries[key] = Entry.makeEntry(null);
            }
        }
        public int getAddress() {
            return address;
        }
        public static int dispatch(final int address, final int n, final int count,
                            final int key) {
            final Keymap km = (Keymap) map.get(new Integer(address));
            Debug.assertTrue(km != null);
            return km.entries[n].getFunction().run(count, key);
        }
    }

    private static native void native_init();

    // Variables
    public static native String rl_line_buffer();
    public static native int rl_point();
    public static native int rl_end();
    public static native int rl_mark();
    public static native int rl_done();
    public static native int rl_pending_input();
    public static native int rl_erase_empty_line();
    public static native String rl_prompt();
    public static native String rl_library_version();
    public static native String rl_readline_name();
    public static native void rl_set_readline_name(String name);
    public static native void rl_add_history(String line);
    // rl_instream
    // rl_outstream
    public static Function rl_startup_hook() {
        return startup_hook;
    }
    public static native void rl_startup_hook(Function hook);
    public static Function rl_pre_input_hook() {
        return pre_input_hook;
    }
    public static native void rl_pre_input_hook(Function hook);
    public static Function rl_event_hook() {
        return event_hook;
    }
    public static native void rl_event_hook(Function hook);
    // rl_getc_function
    // rl_redispaly_function
    public static native Keymap rl_executing_keymap();
    public static native Keymap rl_binding_keymap();

    // Readline main functions -- Readline.c
    public static native String readline(final String prompt);
    public static native int rl_add_defun(final String name,
                                          final Function func,
                                          final int key);

    // Selecting a keymap -- Keymap.c
    public static native Keymap rl_make_bare_keymap();
    public static native Keymap rl_copy_keymap(Keymap map);
    public static native Keymap rl_make_keymap();
    public static native void rl_discard_keymap(Keymap map);
    public static native Keymap rl_get_keymap();
    public static native void rl_set_keymap(Keymap map);
    public static native Keymap rl_get_keymap_by_name(String name);
    public static native String rl_get_keymap_name(Keymap map);

    // Binding keys -- Bind.c
    public static native int rl_bind_key(int key, KeyboardFunction function);
    public static native int rl_bind_key_in_map(int key,
                                                KeyboardFunction function,
                                                Keymap map);
    public static native int rl_unbind_key(int key);
    public static native int rl_unbind_key_in_map(int key, Keymap map);
    public static native int rl_unbind_function_in_map(KeyboardFunction func,
                                                       Keymap map);
    public static native int rl_unbind_command_in_map(String command,
                                                      Keymap map);
    public static native int rl_generic_bind(int type, String keyseq,
                                             String data, Keymap map);
    public static native int rl_parse_and_bind(String line);
    public static native int rl_read_init_file(String filename);

    // Undo -- Undo.c
    public static native int rl_begin_undo_group();
    public static native int rl_end_undogroup();
    public static native void rl_add_undo(int undo_code, int start, int end,
                                          String text);
    public static native void free_undo_list();
    public static native int rl_do_undo();
    public static native int rl_modifying(int start, int end);

    // Redisplay Functions -- Redisplay.c
    public static native void rl_display();
    public static native int rl_forced_update_display();
    public static native int rl_on_new_line();
    public static native int rl_reset_line_state();
    public static native int rl_message(String msg);  // Format string in Java
    public static native int rl_clear_message();
    public static native void rl_save_prompt();
    public static native void rl_restore_prompt();

    // Modifying Text -- Modify.c
    public static native int rl_insert_text(String text);
    public static native int rl_delete_text(int start, int end);
    public static native String rl_copy_text(int start, int end);
    public static native int rl_kill_text(int start, int end);

    // Completion -- Completion.c
    public static native int rl_complete_internal(int what_to_do);
    public static native int rl_complete(int ignore, int key);
    public static native int rl_possible_completions(int count, int key);
    public static native int rl_insert_completions(int count, int key);
    public static native String[] completion_matches(String text,
                                                     CompletionFunction func);
    /** Set the main completion function. **/
    public static native void rl_completion_entry_function(CompletionFunction
                                                           func);
    public static native void rl_completion_attempt_function(AttemptCompletionFunction func);

    // Associating function names and bindings
/*
    public static native KeyboardFunction rl_named_function(String name);
    public static native Function rl_function_of_keyseq(String keyseq,
                                                        Keymap map,
                                                        int *type);
    public static native String[] rl_invoking_keyseqs(KeyboardFunction function);
    public static native String[] rl_invoking_keyseqs_in_map(KeyboardFunction function,
                                                             Keymap map);
    public static native void rl_function_dumper(int readable);
    public static native void rl_list_funmap_names();
*/

}
