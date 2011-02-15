#!/usr/bin/env ruby

#
# Goals:
#
# 1) Simple
#    -No ACL or permissions
#    -Plain pipes / no TCP
#    -Simple commands
#    -Full 8-bit communication
#
# 2) Concurrency and callbacks handle in server
#    -Wait for value
#    -Define callback
#    -Persistent bindings
#
# 3) Use languages to their potential
#    -SEXPR's used for communication to elisp
#
# 4) Simplicity 

=begin

From Ruby to Elisp:

Data is: SEXPR

SEXPR is a LISP list in the form:
(status options elisp)

STATUS is a symbol and is one of:

=   Normal queud message (return the result to elisp)
*   Immediate (handled directly by transaction manager)
      Immediate messages should generally not queue up and, because they are only handled one
      at a time, it isn't possible to use them to perform more complicated elisp nesting.
!   Error with Ruby evaluation.
!!  Internal error. Maybe a bad packet.


OPTIONS is an ALIST with the supported keys

e  Payload will be eval'ed
t  Transaction used; response expected

ELISP is any LISP expression

When the status is = the expression is returned to the invoking
elisp. When the status is * the ELISP is evaluated and the result is
fed back into Ruby.
and I swear there is some kind of iterative memory leak...


From Elisp to Ruby

From elisp to ruby the data is:

HASH

Where supported keys are:

t  Transaction to resume

=end

# basic class structure
class Rel
  class Server
    class RubyEvalError < StandardError; end
    class ElispError < StandardError; end
    class InternalError < StandardError; end

    class PacketError < InternalError; end
    class TransactionError < InternalError; end

    class Log
      def self.info(x)
        STDERR.puts x
      end
      def self.exception(e)
        STDERR.puts "<<ERROR>> " + e.message +
          "\n\t" + e.backtrace.join("\n\t")
      end
    end
  end
end

Log = Rel::Server::Log # XXX quick hack


require 'rel_elisp'
Rel::CoreExt.import_core_extensions


#
# Keeps track of all information for a particular elisp-ruby
# "binding". Continuations are used to allow elisp callbacks.
#
class Rel::Server::Transaction
  RubyEvalError = Rel::Server::RubyEvalError

  # Remove enough to clean up, but not enough
  # to break things...
  instance_methods.each do |m|
    undef_method m unless m =~ /^__/ or
      ["instance_eval"].include? m
  end
  
  def initialize
    @co = nil    # continuation
    @elisp = ""  # contains elisp to run if creating a continuation
    # not implemented, will be used to request how data comes back.
    @data_format = :native
  end

  # *** Methods to be called from inside a running transaction
  
  # Call an elisp function by issuing a Continue. +fn+ can be a string
  # or symbol. +args+ have +to_elisp+ invoked on them.
  def call (fn, *args)
    elisp([fn.to_sym, *args].to_elisp)
  end

  # Like +elisp+ but automatically protects the object passed in which
  # is useful for passing an arbitrary string of elisp to eval. Make
  # sure to call to_elisp if required.
  def elisp (elisp)
    @elisp = Rel::ElispExpression.new(String === elisp ? elisp : elisp.to_elisp)
    _make_continuation
  end

  def with_current_buffer (name, &block)
    wrap(%Q/with-current-buffer "#{name}"/, &block)
  end

  def wrap (elisp)
    elisp = "(progn (" + (String === elisp ? elisp : elisp.to_elisp) + " (+wrap+)) nil)"
    @elisp = Rel::ElispExpression.new(elisp)
    yield _make_continuation
  end

  def method_missing (name, *args, &block)
    if block # need to wrap
      wrap name.to_s.gsub(/_/, '-') + " " + args.to_elisp, &block
    else
      call name, *args
    end
  end

  # *** Internal methods

  # Runs a ruby expression.
  def _run (expr)
    _ruby_eval expr
  end

  def _resume (expr)    
    value = _ruby_eval expr
    @co.call(value)
  end

  def _make_continuation
    val = Kernel.callcc {|@co|
      Kernel.throw :calling_elisp
    }
    @co = nil
    val
  end

  def _pending_elisp
    @elisp
  end

  def _calling_elisp?
    @co and true
  end

  def _ruby_eval (source)
    instance_eval(source)
    # other layers do all the work now
  rescue Exception => e
    Log.exception e
    raise RubyEvalError, e.class.to_s + ":" + e.message, e.backtrace
  end

  def _id; "%x" % __id__.abs; end

end


# 25 FEB 2007 - Removed notion of different 'commands'. This should be
# handled through calling the appropriate Ruby method. KISS.

class Rel::Server
  TO_RUBY_CHUNK_SEP = "\n\n\n"
  FROM_RUBY_CHUNK_SEP = "\n\n\n"

  attr_accessor :in, :out, :manager

  def initialize (io_in, io_out, io_log)
    self.in = io_in
    self.out = io_out
    self.manager = TransactionManager.new
  end

  def start
    Log.info "Server ready: #{Time.now}"
    while input = read
      response =
        begin
          request = Request.new(input)
          Log.info "Recieved data:\n #{request.inspect}"
          handle_request(request)
        rescue RubyEvalError => e
          Log.exception e
          Response.new(e) {|s| s.status = Response::ERROR}
        rescue InternalError => e
          # invalid packet, etc.
          Log.exception e
          Response.new(e) {|s| s.status = Response::INTERNAL_ERROR}
        rescue Exception => e
          # oops, something very bad happened. should not be.
          Log.exception e
          Response.new(InternalError.new(e.message)) {|s| s.status = Reponse::INTERNAL_ERROR}
        end
      write response.to_s
    end
    Log.info "Input stream closed. Exiting."
  end
  
  def handle_request (request)
    transaction =
      if transaction_id = request.options[:transaction]
        manager.find(transaction_id)
      else
        manager.create_transaction
      end
    
    Log.info("Using transaction: #{transaction._id}")
    
    response = run_command(transaction, request.data)

    if response.require_response
      response.transaction_id = transaction._id
      manager.remember(transaction)
    else
      manager.forget(transaction)
    end
    response
  end
  
  def run_command (transaction, ruby_code)
    result = catch :calling_elisp do
      if transaction._calling_elisp?
        transaction._resume(ruby_code)
      else
        transaction._run(ruby_code)
      end
    end
    # we can't just check result because it may be nil/false
    # why not just only accept elisp back?
    if transaction._calling_elisp?
      Response.new(transaction._pending_elisp) do |s|
        s.require_response = true
        s.eval_elisp = true
      end
    else
      Response.new(result) do |s|
        s.eval_elisp = false
      end
    end
  end

  def write (chunk)
    out.print chunk + FROM_RUBY_CHUNK_SEP
  end

  def read
    chunk = self.in.gets(TO_RUBY_CHUNK_SEP)
    unless chunk.nil?
      chunk[0..-(TO_RUBY_CHUNK_SEP.length + 1)]
    end
  end

end


class Rel::Server::Request
  PacketError = Rel::Server::PacketError
  attr_accessor :options, :data

  def initialize (chunk)
    chunk ||= ""

    header, data = chunk.split(":", 2)
    self.options = (header || "").split(",").inject({}) do |memo, option_pair|
      k, v = option_pair.split("=", 2)
      memo[k] = v
      memo
    end
    self.data = data
    self.options[:transaction] = self.options.delete("t")

    unless self.data
      raise PacketError, "Invalid packet '#{chunk[0,30]}'"
    end

  end
end


class Rel::Server::Response
  attr_accessor :status, :data

  NORMAL = :"="
  IMMEDIATE = :"*"
  ERROR = :"!"
  INTERNAL_ERROR = :"!!"

  attr_accessor :transaction_id
  attr_accessor :require_response
  attr_accessor :eval_elisp

  def initialize (data)
    self.data = data
    yield self
#    self.status = options.delete[:status] || NORMAL
#    self.data = options.delete[:data]
#    self.options = options
  end

  # Creates 
  def response_chunk
    options = {}
    options[:t] = transaction_id if transaction_id
    options[:e] = eval_elisp if eval_elisp
    options[:r] = require_response if require_response
    [status || NORMAL, options, data].to_elisp
  end
  alias_method :to_s, :response_chunk
end


class Rel::Server::TransactionManager
  Transaction = Rel::Server::Transaction
  TransactionError = Rel::Server::TransactionError

  attr_accessor :transactions

  def initialize
    self.transactions = {}
  end
  def create_transaction
    Transaction.new
  end
  def find (t_id)
    transactions[t_id] or
      raise(TransactionError, "Transaction '#{t_id}' not found")
  end
  def remember (t)
    transactions[t._id] = t
  end
  def forget (t)
    transactions.delete(t._id)
  end
end


# doesn't return until server is finished
def start_rel_server

  # For some really crummy reason, SIGTERM won't be treated as a kill
  # by default in my particular setup of ruby...
  Kernel.trap("SIGTERM", "DEFAULT")

  if ENV.has_key? "LOG_TO_CONSOLE"
    puts "Logging to console"
  else
    log = File.open("/home/pstickne/rel.log", "w+")
    log.sync = true
    STDERR.reopen(log)
  end

  STDOUT.sync = true
  # require 'stringio'
  # prevent side-effects
  #  io_out = ::STDOUT
  #  Object.class_eval {remove_const :STDOUT}
  #  ::STDOUT = StringIO.new
  
  $server = Rel::Server.new(STDIN, STDOUT, STDERR)
  $server.start
end


if __FILE__ == $0
  start_rel_server
end
