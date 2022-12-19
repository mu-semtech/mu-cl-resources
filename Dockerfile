FROM madnificent/lisp-webservice:0.3.1

COPY . /app
COPY load-config.sh /load-config.sh
COPY startup-resources.sh /startup-resources.sh

ENV BOOT=mu-cl-resources
RUN mkdir /config
RUN sbcl --eval "(progn (ql:update-all-dists :prompt nil) (exit))"
RUN sbcl --load /usr/src/load.lisp

CMD ["/startup-resources.sh"]
