  $ source "$TESTDIR/helper.sh"
  If you don't see this, setup failed

rubocop

  $ run_restyler rubocop --auto-correct -- four.rb
  Inspecting 1 file
  C
  
  Offenses:
  
  four.rb:3:1: C: [Corrected] Layout/IndentationWidth: Use 2 (not 4) spaces for indentation.
      do_something
  ^^^^
  
  1 file inspected, 1 offense detected, 1 offense corrected
  diff --git i/four.rb w/four.rb
  index bcea85b..04d7c60 100644
  --- i/four.rb
  +++ w/four.rb
  @@ -1,4 +1,4 @@
   # bad - four spaces
   def some_method
  -    do_something
  +  do_something
   end

rubocop with incorrectable offenses (anyw8 example)

  $ run_restyler rubocop --auto-correct --fail-level fatal -- andyw8_user.rb
  Inspecting 1 file
  C
  
  Offenses:
  
  andyw8_user.rb:1:1: C: Style/Documentation: Missing top-level class documentation comment.
  class User < ApplicationRecord
  ^^^^^
  andyw8_user.rb:10:34: C: [Corrected] Style/StringLiterals: Prefer single-quoted strings when you don't need string interpolation or special symbols.
    has_many :radars, foreign_key: "owner_id", dependent: :destroy
                                   ^^^^^^^^^^
  andyw8_user.rb:11:42: C: [Corrected] Style/StringLiterals: Prefer single-quoted strings when you don't need string interpolation or special symbols.
    has_many :created_topics, foreign_key: "creator_id", class_name: "Topic", dependent: :nullify
                                           ^^^^^^^^^^^^
  andyw8_user.rb:11:68: C: [Corrected] Style/StringLiterals: Prefer single-quoted strings when you don't need string interpolation or special symbols.
    has_many :created_topics, foreign_key: "creator_id", class_name: "Topic", dependent: :nullify
                                                                     ^^^^^^^
  andyw8_user.rb:11:81: C: Metrics/LineLength: Line is too long. [95/80]
    has_many :created_topics, foreign_key: 'creator_id', class_name: 'Topic', dependent: :nullify
                                                                                  ^^^^^^^^^^^^^^^
  andyw8_user.rb:27:34: C: [Corrected] Style/StringLiterals: Prefer single-quoted strings when you don't need string interpolation or special symbols.
        where(conditions).find_by(["lower(username) = :value OR lower(email) = :value", { value: login.downcase }])
                                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  andyw8_user.rb:27:81: C: Metrics/LineLength: Line is too long. [113/80]
        where(conditions).find_by(['lower(username) = :value OR lower(email) = :value', { value: login.downcase }])
                                                                                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  
  1 file inspected, 7 offenses detected, 4 offenses corrected
  diff --git i/andyw8_user.rb w/andyw8_user.rb
  index ba94afe..45b45f6 100644
  --- i/andyw8_user.rb
  +++ w/andyw8_user.rb
  @@ -7,8 +7,8 @@ class User < ApplicationRecord
     # :confirmable, :lockable, :timeoutable and :omniauthable
     devise :database_authenticatable, :registerable, :confirmable,
            :recoverable, :rememberable, :trackable, :validatable
  -  has_many :radars, foreign_key: "owner_id", dependent: :destroy
  -  has_many :created_topics, foreign_key: "creator_id", class_name: "Topic", dependent: :nullify
  +  has_many :radars, foreign_key: 'owner_id', dependent: :destroy
  +  has_many :created_topics, foreign_key: 'creator_id', class_name: 'Topic', dependent: :nullify
     has_many :blips, through: :radars
   
     attr_accessor :login
  @@ -24,7 +24,7 @@ class User < ApplicationRecord
     def self.find_for_database_authentication(warden_conditions)
       conditions = warden_conditions.dup
       if (login = conditions.delete(:login))
  -      where(conditions).find_by(["lower(username) = :value OR lower(email) = :value", { value: login.downcase }])
  +      where(conditions).find_by(['lower(username) = :value OR lower(email) = :value', { value: login.downcase }])
       else
         # :nocov:
         find_by(conditions)
