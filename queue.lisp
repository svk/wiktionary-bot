(in-package :wiktionary-bot)

(defstruct queue-node
  previous
  next
  value)

(defstruct queue
  head
  tail)

(defun create-queue-node (previous element next)
  (let ((rv (make-queue-node :previous previous
			     :value element
			     :next next)))
    (setf (queue-node-next previous) rv
	  (queue-node-previous next) rv)
    rv))

(defun tear-out-queue-node (node)
  (setf (queue-node-next (queue-node-previous node)) (queue-node-next node)
	(queue-node-previous (queue-node-next node)) (queue-node-previous node))
  node)

(defun create-queue ()
  (let ((head (make-queue-node))
	(tail (make-queue-node)))
    (setf (queue-node-previous tail) head
	  (queue-node-next head) tail)
    (make-queue :head head :tail tail)))

(defun add-to-queue (queue element)
  (create-queue-node (queue-node-previous (queue-tail queue))
		     element
		     (queue-tail queue))
  nil)

(defun add-to-front-of-queue (queue element)
  (create-queue-node (queue-head queue)
		     element
		     (queue-node-next (queue-head queue)))
  nil)

(defun queue-empty? (queue)
  (eq (queue-tail queue)
      (queue-node-next (queue-head queue))))

(defun pop-from-queue (queue)
  (unless (queue-empty? queue)
    (queue-node-value (tear-out-queue-node (queue-node-next (queue-head queue))))))

(defun make-heap-node (&key priority value)
  (cons priority value))

(defun heap-node-value (node) (cdr node))
(defun heap-node-priority (node) (car node))

(defstruct heap
  lock
  dummy
  nodes
  comparator
  number-of-nodes)

(defmacro with-lock (lock &body body)
  #+multiprocessing
  `(mp:with-process-lock (,lock)
     ,@body)
  #-multiprocessing
  `(progn (assert (null ,lock))
	  ,@body))

(defun create-heap (&key (comparator #'<))
  (let ((root (make-heap-node)))
    (make-heap :dummy root
	       :lock
	       #-multiprocessing nil
	       #+multiprocessing (mp:make-process-lock)
	       :nodes (make-array '(1)
				  :initial-contents (list root)
				  :adjustable t)
	       :comparator comparator
	       :number-of-nodes 1)))

(defun clear-heap (heap)
  (with-lock (heap-lock heap)
    (let ((root (make-heap-node)))
      (setf (heap-nodes heap) (make-array '(1)
					  :initial-contents (list root)
					  :adjustable t)
	    (heap-dummy heap) root
	    (heap-number-of-nodes heap) 1)
      nil)))
		


(defparameter *comparisons* 0)

(defun compare-priority (heap alpha beta)
  "Returns true if alpha has higher priority."
  (incf *comparisons*)
  (with-lock (heap-lock heap)
    (cond ((null alpha) nil)
	  ((null beta) t)
	  ((heap-node-dummy? heap alpha) nil)
	  ((heap-node-dummy? heap beta) t)
	  (t (funcall (heap-comparator heap)
		      (heap-node-priority alpha)
		      (heap-node-priority beta))))))

(defun heap-node-dummy? (heap node)
  (with-lock (heap-lock heap)
    (eq (heap-dummy heap) node)))

(defun heap-empty? (heap)
  (with-lock (heap-lock heap)
    (heap-node-dummy? heap (heap-root heap))))

(defun heap-root (heap)
  (with-lock (heap-lock heap)
    (aref (heap-nodes heap) 0)))

(defun parent-index (index)
  (truncate (1- index) 2))

(defparameter *swaps* 0)

(defun swap-nodes (heap swap-to-index index)
  (with-lock (heap-lock heap)
    (incf *swaps*)
    (rotatef (aref (heap-nodes heap) swap-to-index)
	     (aref (heap-nodes heap) index))
    swap-to-index))

(defun add-to-heap (heap priority value)
  (with-lock (heap-lock heap)
    (let ((node (make-heap-node :priority priority
				:value value)))
      (when (> (incf (heap-number-of-nodes heap))
	       (car (array-dimensions (heap-nodes heap))))
	(setf (heap-nodes heap)
	      (adjust-array
	       (heap-nodes heap)
	       (list (* 2 (car (array-dimensions (heap-nodes heap))))))))
      (let ((index (1- (heap-number-of-nodes heap))))
	(setf (aref (heap-nodes heap) index) node)
	(loop
	   :until (or (zerop index)
		      (compare-priority heap
					(aref (heap-nodes heap)
					      (parent-index index))
					(aref (heap-nodes heap)
					      index)))
	   :do (setf index
		     (swap-nodes heap
				 (parent-index index)
				 index)))))))
(defun left-child-index (index)
  (1+ (* 2 index)))

(defun right-child-index (index)
  (+ 2 (* 2 index)))

(defun heap-node-by-index (heap index)
  (with-lock (heap-lock heap)
    (when (< index (heap-number-of-nodes heap))
      (aref (heap-nodes heap) index))))

(defun heap-property-at? (heap i)
  (with-lock (heap-lock heap)
    (let ((root (heap-node-by-index heap i))
	  (left-child (heap-node-by-index heap (left-child-index i)))
	  (right-child (heap-node-by-index heap (right-child-index i))))
      (and (compare-priority heap
			     root
			     left-child)
	   (compare-priority heap
			     root
			     right-child)))))


(defun heap-values (heap)
  (with-lock (heap-lock heap)
    (let ((time (get-universal-time)))
      (sort (mapcar #'(lambda (pairs)
			(cons (- (car pairs) time)
			      (cdr pairs)))
		    (remove-if-not #'identity
				   (remove-if (papply (heap-node-dummy? heap ?))
					      (coerce (heap-nodes heap) 'list))))
	    #'<
	    :key #'car))))

(defun heap-property-violated? (heap)
  (with-lock (heap-lock heap)
    (loop
       :for i :from 0 :below (heap-number-of-nodes heap)
       :if (not (heap-property-at? heap i))
       :collect (list i
		      (heap-node-by-index heap i)
		      (heap-node-by-index heap (left-child-index i))
		      (heap-node-by-index heap (right-child-index i))))))

(defun pop-from-heap (heap &key priority-limit)
  (with-lock (heap-lock heap)
    (when (or (heap-empty? heap)
	      (and priority-limit
		   (funcall (heap-comparator heap)
			    priority-limit
			    (heap-node-priority (heap-root heap)))))
      (return-from pop-from-heap nil))
    (let ((return-value (heap-root heap)))
      (setf (aref (heap-nodes heap) 0) nil)
      (do* ((index (decf (heap-number-of-nodes heap)))
	    (best-child-index 0
			      (if (compare-priority heap
						    (heap-node-by-index
						     heap
						     (left-child-index index))
						    (heap-node-by-index
						     heap
						     (right-child-index index)))
				  (left-child-index index)
				  (right-child-index index))))
	   ((or (>= best-child-index (heap-number-of-nodes heap))
		(and (not (zerop best-child-index))
		     (compare-priority heap
				       (aref (heap-nodes heap) index)
				       (aref (heap-nodes heap)
					     best-child-index)))))
	#+foo
	(log-test 'pop-from-heap
		  "must swap [we are ~a ~a, children ~a ~a and ~a ~a, swapping with ~a, comparisons ~a, swaps ~a, size ~a"
		  index
		  (aref (heap-nodes heap) index)
		  (left-child-index index)
		  (heap-node-by-index heap (left-child-index index))
		  (right-child-index index)
		  (heap-node-by-index heap (right-child-index index))
		  best-child-index
		  *comparisons*
		  *swaps*
		  (heap-number-of-nodes heap))
	(setf index (swap-nodes heap best-child-index index)))
      #+fkd
      (log-test 'pop-from-heap
		"returning")
      (heap-node-value return-value))))
	       
(defun test-sort-by-heap (&key (n 10) list check print)
  (let ((list (or list (randomize-list (loop :for i :from 1 :to n :collect i))))
	(heap (create-heap)))
    (let ((*comparisons* 0)
	  (*swaps* 0))
      (dolist (element list)
	(add-to-heap heap element (format nil "[~a]" element))
	(unless (or (not check)
		    (not (heap-property-violated? heap)))
	  (log-test 'test-sort-by-heap
		    "heap property violated in test after insert ~a: ~a"
		    element
		    (heap-property-violated? heap))))
      (log-test 'test-sort-by-heap "insertions done, ~a comparisons, ~a swaps" *comparisons* *swaps*))
    (let ((*comparisons* 0)
	  (*swaps* 0))
      (loop
	 :until (heap-empty? heap)
	 :do (let ((element (pop-from-heap heap)))
	       (when print (format t "~a~%" element))))
      (log-test 'test-sort-by-heap "removals done, ~a comparisons, ~a swaps" *comparisons* *swaps*))))

(defun test-heapsort (list &optional limit)
  (let ((heap (create-heap)))
    (dolist (element list)
      (add-to-heap heap element element))
    (loop
       :for x = (pop-from-heap heap :priority-limit limit)
       :until (null x)
       :collect x)))
       
