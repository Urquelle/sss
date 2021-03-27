struct Queue_Entry {
    Queue_Entry * next;
    Queue_Entry * prev;
    void        * data;
};

struct Queue {
    Queue_Entry   root;
    Queue_Entry * curr;
    size_t        num_elems;
};

void *
queue_entry(Queue *q, size_t index) {
    if ( index >= q->num_elems ) {
        return NULL;
    }

    Queue_Entry *elem = q->root.next;
    for ( int i = 0; i < index; ++i ) {
        elem = elem->next;
    }

    return elem->data;
}

void
queue_push(Queue *q, void *data) {
    Queue_Entry *entry = urq_allocs(Queue_Entry);

    if ( !q->curr ) {
        q->curr = &q->root;
    }

    entry->data = data;
    entry->next = NULL;
    entry->prev = q->curr;

    q->curr->next = entry;
    q->curr = entry;
    q->num_elems++;
}

void *
queue_pop(Queue *q) {
    void *result = q->curr->data;

    if ( q->curr != &q->root) {
        q->curr = q->curr->prev;
    }

    q->num_elems--;

    return result;
}

void *
queue_shift(Queue *q) {
    void *result = 0;

    if ( !q->root.next ) {
        return result;
    }

    result = q->root.next->data;
    q->root.next = q->root.next->next;

    if ( q->root.next ) {
        q->root.next->prev = &q->root;
    }

    q->num_elems--;

    return result;
}

void
queue_unshift(Queue *q, void *data) {
    Queue_Entry *entry = urq_allocs(Queue_Entry);

    entry->data = data;
    entry->next = q->root.next;
    entry->prev = &q->root;

    q->root.next = entry;
    q->num_elems++;
}

void
queue_remove(Queue *q, void *data) {
    Queue_Entry *elem = q->root.next;
    for ( int i = 0; i < q->num_elems; ++i ) {
        elem = elem->next;

        if ( elem->data == data ) {
            elem->prev->next = elem->next;
            elem->next->prev = elem->prev;
            q->num_elems--;

            break;
        }
    }
}

