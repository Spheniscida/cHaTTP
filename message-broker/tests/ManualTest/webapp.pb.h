// Generated by the protocol buffer compiler.  DO NOT EDIT!
// source: webapp.proto

#ifndef PROTOBUF_webapp_2eproto__INCLUDED
#define PROTOBUF_webapp_2eproto__INCLUDED

#include <string>

#include <google/protobuf/stubs/common.h>

#if GOOGLE_PROTOBUF_VERSION < 2005000
#error This file was generated by a newer version of protoc which is
#error incompatible with your Protocol Buffer headers.  Please update
#error your headers.
#endif
#if 2005000 < GOOGLE_PROTOBUF_MIN_PROTOC_VERSION
#error This file was generated by an older version of protoc which is
#error incompatible with your Protocol Buffer headers.  Please
#error regenerate this file with a newer version of protoc.
#endif

#include <google/protobuf/generated_message_util.h>
#include <google/protobuf/message.h>
#include <google/protobuf/repeated_field.h>
#include <google/protobuf/extension_set.h>
#include <google/protobuf/generated_enum_reflection.h>
#include <google/protobuf/unknown_field_set.h>
#include "message.pb.h"
// @@protoc_insertion_point(includes)

namespace chattp {

// Internal implementation detail -- do not call these.
void  protobuf_AddDesc_webapp_2eproto();
void protobuf_AssignDesc_webapp_2eproto();
void protobuf_ShutdownFile_webapp_2eproto();

class WebappRequest;
class WebappResponse;

enum WebappRequest_WebappRequestType {
  WebappRequest_WebappRequestType_REGISTER = 0,
  WebappRequest_WebappRequestType_LOGIN = 1,
  WebappRequest_WebappRequestType_LOGOUT = 2,
  WebappRequest_WebappRequestType_SENDMESSAGE = 3,
  WebappRequest_WebappRequestType_QUERYSTATUS = 4,
  WebappRequest_WebappRequestType_GETMESSAGES = 5,
  WebappRequest_WebappRequestType_AUTHORIZED = 6
};
bool WebappRequest_WebappRequestType_IsValid(int value);
const WebappRequest_WebappRequestType WebappRequest_WebappRequestType_WebappRequestType_MIN = WebappRequest_WebappRequestType_REGISTER;
const WebappRequest_WebappRequestType WebappRequest_WebappRequestType_WebappRequestType_MAX = WebappRequest_WebappRequestType_AUTHORIZED;
const int WebappRequest_WebappRequestType_WebappRequestType_ARRAYSIZE = WebappRequest_WebappRequestType_WebappRequestType_MAX + 1;

const ::google::protobuf::EnumDescriptor* WebappRequest_WebappRequestType_descriptor();
inline const ::std::string& WebappRequest_WebappRequestType_Name(WebappRequest_WebappRequestType value) {
  return ::google::protobuf::internal::NameOfEnum(
    WebappRequest_WebappRequestType_descriptor(), value);
}
inline bool WebappRequest_WebappRequestType_Parse(
    const ::std::string& name, WebappRequest_WebappRequestType* value) {
  return ::google::protobuf::internal::ParseNamedEnum<WebappRequest_WebappRequestType>(
    WebappRequest_WebappRequestType_descriptor(), name, value);
}
enum WebappResponse_WebappResponseType {
  WebappResponse_WebappResponseType_REGISTERED = 0,
  WebappResponse_WebappResponseType_LOGGEDIN = 1,
  WebappResponse_WebappResponseType_LOGGEDOUT = 2,
  WebappResponse_WebappResponseType_SENTMESSAGE = 3,
  WebappResponse_WebappResponseType_USERSTATUS = 4,
  WebappResponse_WebappResponseType_GOTMESSAGES = 5,
  WebappResponse_WebappResponseType_AUTHORIZED = 6
};
bool WebappResponse_WebappResponseType_IsValid(int value);
const WebappResponse_WebappResponseType WebappResponse_WebappResponseType_WebappResponseType_MIN = WebappResponse_WebappResponseType_REGISTERED;
const WebappResponse_WebappResponseType WebappResponse_WebappResponseType_WebappResponseType_MAX = WebappResponse_WebappResponseType_AUTHORIZED;
const int WebappResponse_WebappResponseType_WebappResponseType_ARRAYSIZE = WebappResponse_WebappResponseType_WebappResponseType_MAX + 1;

const ::google::protobuf::EnumDescriptor* WebappResponse_WebappResponseType_descriptor();
inline const ::std::string& WebappResponse_WebappResponseType_Name(WebappResponse_WebappResponseType value) {
  return ::google::protobuf::internal::NameOfEnum(
    WebappResponse_WebappResponseType_descriptor(), value);
}
inline bool WebappResponse_WebappResponseType_Parse(
    const ::std::string& name, WebappResponse_WebappResponseType* value) {
  return ::google::protobuf::internal::ParseNamedEnum<WebappResponse_WebappResponseType>(
    WebappResponse_WebappResponseType_descriptor(), name, value);
}
// ===================================================================

class WebappRequest : public ::google::protobuf::Message {
 public:
  WebappRequest();
  virtual ~WebappRequest();

  WebappRequest(const WebappRequest& from);

  inline WebappRequest& operator=(const WebappRequest& from) {
    CopyFrom(from);
    return *this;
  }

  inline const ::google::protobuf::UnknownFieldSet& unknown_fields() const {
    return _unknown_fields_;
  }

  inline ::google::protobuf::UnknownFieldSet* mutable_unknown_fields() {
    return &_unknown_fields_;
  }

  static const ::google::protobuf::Descriptor* descriptor();
  static const WebappRequest& default_instance();

  void Swap(WebappRequest* other);

  // implements Message ----------------------------------------------

  WebappRequest* New() const;
  void CopyFrom(const ::google::protobuf::Message& from);
  void MergeFrom(const ::google::protobuf::Message& from);
  void CopyFrom(const WebappRequest& from);
  void MergeFrom(const WebappRequest& from);
  void Clear();
  bool IsInitialized() const;

  int ByteSize() const;
  bool MergePartialFromCodedStream(
      ::google::protobuf::io::CodedInputStream* input);
  void SerializeWithCachedSizes(
      ::google::protobuf::io::CodedOutputStream* output) const;
  ::google::protobuf::uint8* SerializeWithCachedSizesToArray(::google::protobuf::uint8* output) const;
  int GetCachedSize() const { return _cached_size_; }
  private:
  void SharedCtor();
  void SharedDtor();
  void SetCachedSize(int size) const;
  public:

  ::google::protobuf::Metadata GetMetadata() const;

  // nested types ----------------------------------------------------

  typedef WebappRequest_WebappRequestType WebappRequestType;
  static const WebappRequestType REGISTER = WebappRequest_WebappRequestType_REGISTER;
  static const WebappRequestType LOGIN = WebappRequest_WebappRequestType_LOGIN;
  static const WebappRequestType LOGOUT = WebappRequest_WebappRequestType_LOGOUT;
  static const WebappRequestType SENDMESSAGE = WebappRequest_WebappRequestType_SENDMESSAGE;
  static const WebappRequestType QUERYSTATUS = WebappRequest_WebappRequestType_QUERYSTATUS;
  static const WebappRequestType GETMESSAGES = WebappRequest_WebappRequestType_GETMESSAGES;
  static const WebappRequestType AUTHORIZED = WebappRequest_WebappRequestType_AUTHORIZED;
  static inline bool WebappRequestType_IsValid(int value) {
    return WebappRequest_WebappRequestType_IsValid(value);
  }
  static const WebappRequestType WebappRequestType_MIN =
    WebappRequest_WebappRequestType_WebappRequestType_MIN;
  static const WebappRequestType WebappRequestType_MAX =
    WebappRequest_WebappRequestType_WebappRequestType_MAX;
  static const int WebappRequestType_ARRAYSIZE =
    WebappRequest_WebappRequestType_WebappRequestType_ARRAYSIZE;
  static inline const ::google::protobuf::EnumDescriptor*
  WebappRequestType_descriptor() {
    return WebappRequest_WebappRequestType_descriptor();
  }
  static inline const ::std::string& WebappRequestType_Name(WebappRequestType value) {
    return WebappRequest_WebappRequestType_Name(value);
  }
  static inline bool WebappRequestType_Parse(const ::std::string& name,
      WebappRequestType* value) {
    return WebappRequest_WebappRequestType_Parse(name, value);
  }

  // accessors -------------------------------------------------------

  // required uint64 sequence_number = 1;
  inline bool has_sequence_number() const;
  inline void clear_sequence_number();
  static const int kSequenceNumberFieldNumber = 1;
  inline ::google::protobuf::uint64 sequence_number() const;
  inline void set_sequence_number(::google::protobuf::uint64 value);

  // required .chattp.WebappRequest.WebappRequestType type = 2;
  inline bool has_type() const;
  inline void clear_type();
  static const int kTypeFieldNumber = 2;
  inline ::chattp::WebappRequest_WebappRequestType type() const;
  inline void set_type(::chattp::WebappRequest_WebappRequestType value);

  // optional string user_name = 3;
  inline bool has_user_name() const;
  inline void clear_user_name();
  static const int kUserNameFieldNumber = 3;
  inline const ::std::string& user_name() const;
  inline void set_user_name(const ::std::string& value);
  inline void set_user_name(const char* value);
  inline void set_user_name(const char* value, size_t size);
  inline ::std::string* mutable_user_name();
  inline ::std::string* release_user_name();
  inline void set_allocated_user_name(::std::string* user_name);

  // optional string password = 4;
  inline bool has_password() const;
  inline void clear_password();
  static const int kPasswordFieldNumber = 4;
  inline const ::std::string& password() const;
  inline void set_password(const ::std::string& value);
  inline void set_password(const char* value);
  inline void set_password(const char* value, size_t size);
  inline ::std::string* mutable_password();
  inline ::std::string* release_password();
  inline void set_allocated_password(::std::string* password);

  // optional string channel_id = 5;
  inline bool has_channel_id() const;
  inline void clear_channel_id();
  static const int kChannelIdFieldNumber = 5;
  inline const ::std::string& channel_id() const;
  inline void set_channel_id(const ::std::string& value);
  inline void set_channel_id(const char* value);
  inline void set_channel_id(const char* value, size_t size);
  inline ::std::string* mutable_channel_id();
  inline ::std::string* release_channel_id();
  inline void set_allocated_channel_id(::std::string* channel_id);

  // optional .chattp.ChattpMessage mesg = 6;
  inline bool has_mesg() const;
  inline void clear_mesg();
  static const int kMesgFieldNumber = 6;
  inline const ::chattp::ChattpMessage& mesg() const;
  inline ::chattp::ChattpMessage* mutable_mesg();
  inline ::chattp::ChattpMessage* release_mesg();
  inline void set_allocated_mesg(::chattp::ChattpMessage* mesg);

  // @@protoc_insertion_point(class_scope:chattp.WebappRequest)
 private:
  inline void set_has_sequence_number();
  inline void clear_has_sequence_number();
  inline void set_has_type();
  inline void clear_has_type();
  inline void set_has_user_name();
  inline void clear_has_user_name();
  inline void set_has_password();
  inline void clear_has_password();
  inline void set_has_channel_id();
  inline void clear_has_channel_id();
  inline void set_has_mesg();
  inline void clear_has_mesg();

  ::google::protobuf::UnknownFieldSet _unknown_fields_;

  ::google::protobuf::uint64 sequence_number_;
  ::std::string* user_name_;
  ::std::string* password_;
  ::std::string* channel_id_;
  ::chattp::ChattpMessage* mesg_;
  int type_;

  mutable int _cached_size_;
  ::google::protobuf::uint32 _has_bits_[(6 + 31) / 32];

  friend void  protobuf_AddDesc_webapp_2eproto();
  friend void protobuf_AssignDesc_webapp_2eproto();
  friend void protobuf_ShutdownFile_webapp_2eproto();

  void InitAsDefaultInstance();
  static WebappRequest* default_instance_;
};
// -------------------------------------------------------------------

class WebappResponse : public ::google::protobuf::Message {
 public:
  WebappResponse();
  virtual ~WebappResponse();

  WebappResponse(const WebappResponse& from);

  inline WebappResponse& operator=(const WebappResponse& from) {
    CopyFrom(from);
    return *this;
  }

  inline const ::google::protobuf::UnknownFieldSet& unknown_fields() const {
    return _unknown_fields_;
  }

  inline ::google::protobuf::UnknownFieldSet* mutable_unknown_fields() {
    return &_unknown_fields_;
  }

  static const ::google::protobuf::Descriptor* descriptor();
  static const WebappResponse& default_instance();

  void Swap(WebappResponse* other);

  // implements Message ----------------------------------------------

  WebappResponse* New() const;
  void CopyFrom(const ::google::protobuf::Message& from);
  void MergeFrom(const ::google::protobuf::Message& from);
  void CopyFrom(const WebappResponse& from);
  void MergeFrom(const WebappResponse& from);
  void Clear();
  bool IsInitialized() const;

  int ByteSize() const;
  bool MergePartialFromCodedStream(
      ::google::protobuf::io::CodedInputStream* input);
  void SerializeWithCachedSizes(
      ::google::protobuf::io::CodedOutputStream* output) const;
  ::google::protobuf::uint8* SerializeWithCachedSizesToArray(::google::protobuf::uint8* output) const;
  int GetCachedSize() const { return _cached_size_; }
  private:
  void SharedCtor();
  void SharedDtor();
  void SetCachedSize(int size) const;
  public:

  ::google::protobuf::Metadata GetMetadata() const;

  // nested types ----------------------------------------------------

  typedef WebappResponse_WebappResponseType WebappResponseType;
  static const WebappResponseType REGISTERED = WebappResponse_WebappResponseType_REGISTERED;
  static const WebappResponseType LOGGEDIN = WebappResponse_WebappResponseType_LOGGEDIN;
  static const WebappResponseType LOGGEDOUT = WebappResponse_WebappResponseType_LOGGEDOUT;
  static const WebappResponseType SENTMESSAGE = WebappResponse_WebappResponseType_SENTMESSAGE;
  static const WebappResponseType USERSTATUS = WebappResponse_WebappResponseType_USERSTATUS;
  static const WebappResponseType GOTMESSAGES = WebappResponse_WebappResponseType_GOTMESSAGES;
  static const WebappResponseType AUTHORIZED = WebappResponse_WebappResponseType_AUTHORIZED;
  static inline bool WebappResponseType_IsValid(int value) {
    return WebappResponse_WebappResponseType_IsValid(value);
  }
  static const WebappResponseType WebappResponseType_MIN =
    WebappResponse_WebappResponseType_WebappResponseType_MIN;
  static const WebappResponseType WebappResponseType_MAX =
    WebappResponse_WebappResponseType_WebappResponseType_MAX;
  static const int WebappResponseType_ARRAYSIZE =
    WebappResponse_WebappResponseType_WebappResponseType_ARRAYSIZE;
  static inline const ::google::protobuf::EnumDescriptor*
  WebappResponseType_descriptor() {
    return WebappResponse_WebappResponseType_descriptor();
  }
  static inline const ::std::string& WebappResponseType_Name(WebappResponseType value) {
    return WebappResponse_WebappResponseType_Name(value);
  }
  static inline bool WebappResponseType_Parse(const ::std::string& name,
      WebappResponseType* value) {
    return WebappResponse_WebappResponseType_Parse(name, value);
  }

  // accessors -------------------------------------------------------

  // required uint64 sequence_number = 1;
  inline bool has_sequence_number() const;
  inline void clear_sequence_number();
  static const int kSequenceNumberFieldNumber = 1;
  inline ::google::protobuf::uint64 sequence_number() const;
  inline void set_sequence_number(::google::protobuf::uint64 value);

  // required .chattp.WebappResponse.WebappResponseType type = 2;
  inline bool has_type() const;
  inline void clear_type();
  static const int kTypeFieldNumber = 2;
  inline ::chattp::WebappResponse_WebappResponseType type() const;
  inline void set_type(::chattp::WebappResponse_WebappResponseType value);

  // optional bool status = 3 [default = true];
  inline bool has_status() const;
  inline void clear_status();
  static const int kStatusFieldNumber = 3;
  inline bool status() const;
  inline void set_status(bool value);

  // optional bool online = 4;
  inline bool has_online() const;
  inline void clear_online();
  static const int kOnlineFieldNumber = 4;
  inline bool online() const;
  inline void set_online(bool value);

  // optional bool authorized = 5;
  inline bool has_authorized() const;
  inline void clear_authorized();
  static const int kAuthorizedFieldNumber = 5;
  inline bool authorized() const;
  inline void set_authorized(bool value);

  // optional string channel_id = 6;
  inline bool has_channel_id() const;
  inline void clear_channel_id();
  static const int kChannelIdFieldNumber = 6;
  inline const ::std::string& channel_id() const;
  inline void set_channel_id(const ::std::string& value);
  inline void set_channel_id(const char* value);
  inline void set_channel_id(const char* value, size_t size);
  inline ::std::string* mutable_channel_id();
  inline ::std::string* release_channel_id();
  inline void set_allocated_channel_id(::std::string* channel_id);

  // repeated .chattp.ChattpMessage mesgs = 7;
  inline int mesgs_size() const;
  inline void clear_mesgs();
  static const int kMesgsFieldNumber = 7;
  inline const ::chattp::ChattpMessage& mesgs(int index) const;
  inline ::chattp::ChattpMessage* mutable_mesgs(int index);
  inline ::chattp::ChattpMessage* add_mesgs();
  inline const ::google::protobuf::RepeatedPtrField< ::chattp::ChattpMessage >&
      mesgs() const;
  inline ::google::protobuf::RepeatedPtrField< ::chattp::ChattpMessage >*
      mutable_mesgs();

  // @@protoc_insertion_point(class_scope:chattp.WebappResponse)
 private:
  inline void set_has_sequence_number();
  inline void clear_has_sequence_number();
  inline void set_has_type();
  inline void clear_has_type();
  inline void set_has_status();
  inline void clear_has_status();
  inline void set_has_online();
  inline void clear_has_online();
  inline void set_has_authorized();
  inline void clear_has_authorized();
  inline void set_has_channel_id();
  inline void clear_has_channel_id();

  ::google::protobuf::UnknownFieldSet _unknown_fields_;

  ::google::protobuf::uint64 sequence_number_;
  int type_;
  bool status_;
  bool online_;
  bool authorized_;
  ::std::string* channel_id_;
  ::google::protobuf::RepeatedPtrField< ::chattp::ChattpMessage > mesgs_;

  mutable int _cached_size_;
  ::google::protobuf::uint32 _has_bits_[(7 + 31) / 32];

  friend void  protobuf_AddDesc_webapp_2eproto();
  friend void protobuf_AssignDesc_webapp_2eproto();
  friend void protobuf_ShutdownFile_webapp_2eproto();

  void InitAsDefaultInstance();
  static WebappResponse* default_instance_;
};
// ===================================================================


// ===================================================================

// WebappRequest

// required uint64 sequence_number = 1;
inline bool WebappRequest::has_sequence_number() const {
  return (_has_bits_[0] & 0x00000001u) != 0;
}
inline void WebappRequest::set_has_sequence_number() {
  _has_bits_[0] |= 0x00000001u;
}
inline void WebappRequest::clear_has_sequence_number() {
  _has_bits_[0] &= ~0x00000001u;
}
inline void WebappRequest::clear_sequence_number() {
  sequence_number_ = GOOGLE_ULONGLONG(0);
  clear_has_sequence_number();
}
inline ::google::protobuf::uint64 WebappRequest::sequence_number() const {
  return sequence_number_;
}
inline void WebappRequest::set_sequence_number(::google::protobuf::uint64 value) {
  set_has_sequence_number();
  sequence_number_ = value;
}

// required .chattp.WebappRequest.WebappRequestType type = 2;
inline bool WebappRequest::has_type() const {
  return (_has_bits_[0] & 0x00000002u) != 0;
}
inline void WebappRequest::set_has_type() {
  _has_bits_[0] |= 0x00000002u;
}
inline void WebappRequest::clear_has_type() {
  _has_bits_[0] &= ~0x00000002u;
}
inline void WebappRequest::clear_type() {
  type_ = 0;
  clear_has_type();
}
inline ::chattp::WebappRequest_WebappRequestType WebappRequest::type() const {
  return static_cast< ::chattp::WebappRequest_WebappRequestType >(type_);
}
inline void WebappRequest::set_type(::chattp::WebappRequest_WebappRequestType value) {
  assert(::chattp::WebappRequest_WebappRequestType_IsValid(value));
  set_has_type();
  type_ = value;
}

// optional string user_name = 3;
inline bool WebappRequest::has_user_name() const {
  return (_has_bits_[0] & 0x00000004u) != 0;
}
inline void WebappRequest::set_has_user_name() {
  _has_bits_[0] |= 0x00000004u;
}
inline void WebappRequest::clear_has_user_name() {
  _has_bits_[0] &= ~0x00000004u;
}
inline void WebappRequest::clear_user_name() {
  if (user_name_ != &::google::protobuf::internal::kEmptyString) {
    user_name_->clear();
  }
  clear_has_user_name();
}
inline const ::std::string& WebappRequest::user_name() const {
  return *user_name_;
}
inline void WebappRequest::set_user_name(const ::std::string& value) {
  set_has_user_name();
  if (user_name_ == &::google::protobuf::internal::kEmptyString) {
    user_name_ = new ::std::string;
  }
  user_name_->assign(value);
}
inline void WebappRequest::set_user_name(const char* value) {
  set_has_user_name();
  if (user_name_ == &::google::protobuf::internal::kEmptyString) {
    user_name_ = new ::std::string;
  }
  user_name_->assign(value);
}
inline void WebappRequest::set_user_name(const char* value, size_t size) {
  set_has_user_name();
  if (user_name_ == &::google::protobuf::internal::kEmptyString) {
    user_name_ = new ::std::string;
  }
  user_name_->assign(reinterpret_cast<const char*>(value), size);
}
inline ::std::string* WebappRequest::mutable_user_name() {
  set_has_user_name();
  if (user_name_ == &::google::protobuf::internal::kEmptyString) {
    user_name_ = new ::std::string;
  }
  return user_name_;
}
inline ::std::string* WebappRequest::release_user_name() {
  clear_has_user_name();
  if (user_name_ == &::google::protobuf::internal::kEmptyString) {
    return NULL;
  } else {
    ::std::string* temp = user_name_;
    user_name_ = const_cast< ::std::string*>(&::google::protobuf::internal::kEmptyString);
    return temp;
  }
}
inline void WebappRequest::set_allocated_user_name(::std::string* user_name) {
  if (user_name_ != &::google::protobuf::internal::kEmptyString) {
    delete user_name_;
  }
  if (user_name) {
    set_has_user_name();
    user_name_ = user_name;
  } else {
    clear_has_user_name();
    user_name_ = const_cast< ::std::string*>(&::google::protobuf::internal::kEmptyString);
  }
}

// optional string password = 4;
inline bool WebappRequest::has_password() const {
  return (_has_bits_[0] & 0x00000008u) != 0;
}
inline void WebappRequest::set_has_password() {
  _has_bits_[0] |= 0x00000008u;
}
inline void WebappRequest::clear_has_password() {
  _has_bits_[0] &= ~0x00000008u;
}
inline void WebappRequest::clear_password() {
  if (password_ != &::google::protobuf::internal::kEmptyString) {
    password_->clear();
  }
  clear_has_password();
}
inline const ::std::string& WebappRequest::password() const {
  return *password_;
}
inline void WebappRequest::set_password(const ::std::string& value) {
  set_has_password();
  if (password_ == &::google::protobuf::internal::kEmptyString) {
    password_ = new ::std::string;
  }
  password_->assign(value);
}
inline void WebappRequest::set_password(const char* value) {
  set_has_password();
  if (password_ == &::google::protobuf::internal::kEmptyString) {
    password_ = new ::std::string;
  }
  password_->assign(value);
}
inline void WebappRequest::set_password(const char* value, size_t size) {
  set_has_password();
  if (password_ == &::google::protobuf::internal::kEmptyString) {
    password_ = new ::std::string;
  }
  password_->assign(reinterpret_cast<const char*>(value), size);
}
inline ::std::string* WebappRequest::mutable_password() {
  set_has_password();
  if (password_ == &::google::protobuf::internal::kEmptyString) {
    password_ = new ::std::string;
  }
  return password_;
}
inline ::std::string* WebappRequest::release_password() {
  clear_has_password();
  if (password_ == &::google::protobuf::internal::kEmptyString) {
    return NULL;
  } else {
    ::std::string* temp = password_;
    password_ = const_cast< ::std::string*>(&::google::protobuf::internal::kEmptyString);
    return temp;
  }
}
inline void WebappRequest::set_allocated_password(::std::string* password) {
  if (password_ != &::google::protobuf::internal::kEmptyString) {
    delete password_;
  }
  if (password) {
    set_has_password();
    password_ = password;
  } else {
    clear_has_password();
    password_ = const_cast< ::std::string*>(&::google::protobuf::internal::kEmptyString);
  }
}

// optional string channel_id = 5;
inline bool WebappRequest::has_channel_id() const {
  return (_has_bits_[0] & 0x00000010u) != 0;
}
inline void WebappRequest::set_has_channel_id() {
  _has_bits_[0] |= 0x00000010u;
}
inline void WebappRequest::clear_has_channel_id() {
  _has_bits_[0] &= ~0x00000010u;
}
inline void WebappRequest::clear_channel_id() {
  if (channel_id_ != &::google::protobuf::internal::kEmptyString) {
    channel_id_->clear();
  }
  clear_has_channel_id();
}
inline const ::std::string& WebappRequest::channel_id() const {
  return *channel_id_;
}
inline void WebappRequest::set_channel_id(const ::std::string& value) {
  set_has_channel_id();
  if (channel_id_ == &::google::protobuf::internal::kEmptyString) {
    channel_id_ = new ::std::string;
  }
  channel_id_->assign(value);
}
inline void WebappRequest::set_channel_id(const char* value) {
  set_has_channel_id();
  if (channel_id_ == &::google::protobuf::internal::kEmptyString) {
    channel_id_ = new ::std::string;
  }
  channel_id_->assign(value);
}
inline void WebappRequest::set_channel_id(const char* value, size_t size) {
  set_has_channel_id();
  if (channel_id_ == &::google::protobuf::internal::kEmptyString) {
    channel_id_ = new ::std::string;
  }
  channel_id_->assign(reinterpret_cast<const char*>(value), size);
}
inline ::std::string* WebappRequest::mutable_channel_id() {
  set_has_channel_id();
  if (channel_id_ == &::google::protobuf::internal::kEmptyString) {
    channel_id_ = new ::std::string;
  }
  return channel_id_;
}
inline ::std::string* WebappRequest::release_channel_id() {
  clear_has_channel_id();
  if (channel_id_ == &::google::protobuf::internal::kEmptyString) {
    return NULL;
  } else {
    ::std::string* temp = channel_id_;
    channel_id_ = const_cast< ::std::string*>(&::google::protobuf::internal::kEmptyString);
    return temp;
  }
}
inline void WebappRequest::set_allocated_channel_id(::std::string* channel_id) {
  if (channel_id_ != &::google::protobuf::internal::kEmptyString) {
    delete channel_id_;
  }
  if (channel_id) {
    set_has_channel_id();
    channel_id_ = channel_id;
  } else {
    clear_has_channel_id();
    channel_id_ = const_cast< ::std::string*>(&::google::protobuf::internal::kEmptyString);
  }
}

// optional .chattp.ChattpMessage mesg = 6;
inline bool WebappRequest::has_mesg() const {
  return (_has_bits_[0] & 0x00000020u) != 0;
}
inline void WebappRequest::set_has_mesg() {
  _has_bits_[0] |= 0x00000020u;
}
inline void WebappRequest::clear_has_mesg() {
  _has_bits_[0] &= ~0x00000020u;
}
inline void WebappRequest::clear_mesg() {
  if (mesg_ != NULL) mesg_->::chattp::ChattpMessage::Clear();
  clear_has_mesg();
}
inline const ::chattp::ChattpMessage& WebappRequest::mesg() const {
  return mesg_ != NULL ? *mesg_ : *default_instance_->mesg_;
}
inline ::chattp::ChattpMessage* WebappRequest::mutable_mesg() {
  set_has_mesg();
  if (mesg_ == NULL) mesg_ = new ::chattp::ChattpMessage;
  return mesg_;
}
inline ::chattp::ChattpMessage* WebappRequest::release_mesg() {
  clear_has_mesg();
  ::chattp::ChattpMessage* temp = mesg_;
  mesg_ = NULL;
  return temp;
}
inline void WebappRequest::set_allocated_mesg(::chattp::ChattpMessage* mesg) {
  delete mesg_;
  mesg_ = mesg;
  if (mesg) {
    set_has_mesg();
  } else {
    clear_has_mesg();
  }
}

// -------------------------------------------------------------------

// WebappResponse

// required uint64 sequence_number = 1;
inline bool WebappResponse::has_sequence_number() const {
  return (_has_bits_[0] & 0x00000001u) != 0;
}
inline void WebappResponse::set_has_sequence_number() {
  _has_bits_[0] |= 0x00000001u;
}
inline void WebappResponse::clear_has_sequence_number() {
  _has_bits_[0] &= ~0x00000001u;
}
inline void WebappResponse::clear_sequence_number() {
  sequence_number_ = GOOGLE_ULONGLONG(0);
  clear_has_sequence_number();
}
inline ::google::protobuf::uint64 WebappResponse::sequence_number() const {
  return sequence_number_;
}
inline void WebappResponse::set_sequence_number(::google::protobuf::uint64 value) {
  set_has_sequence_number();
  sequence_number_ = value;
}

// required .chattp.WebappResponse.WebappResponseType type = 2;
inline bool WebappResponse::has_type() const {
  return (_has_bits_[0] & 0x00000002u) != 0;
}
inline void WebappResponse::set_has_type() {
  _has_bits_[0] |= 0x00000002u;
}
inline void WebappResponse::clear_has_type() {
  _has_bits_[0] &= ~0x00000002u;
}
inline void WebappResponse::clear_type() {
  type_ = 0;
  clear_has_type();
}
inline ::chattp::WebappResponse_WebappResponseType WebappResponse::type() const {
  return static_cast< ::chattp::WebappResponse_WebappResponseType >(type_);
}
inline void WebappResponse::set_type(::chattp::WebappResponse_WebappResponseType value) {
  assert(::chattp::WebappResponse_WebappResponseType_IsValid(value));
  set_has_type();
  type_ = value;
}

// optional bool status = 3 [default = true];
inline bool WebappResponse::has_status() const {
  return (_has_bits_[0] & 0x00000004u) != 0;
}
inline void WebappResponse::set_has_status() {
  _has_bits_[0] |= 0x00000004u;
}
inline void WebappResponse::clear_has_status() {
  _has_bits_[0] &= ~0x00000004u;
}
inline void WebappResponse::clear_status() {
  status_ = true;
  clear_has_status();
}
inline bool WebappResponse::status() const {
  return status_;
}
inline void WebappResponse::set_status(bool value) {
  set_has_status();
  status_ = value;
}

// optional bool online = 4;
inline bool WebappResponse::has_online() const {
  return (_has_bits_[0] & 0x00000008u) != 0;
}
inline void WebappResponse::set_has_online() {
  _has_bits_[0] |= 0x00000008u;
}
inline void WebappResponse::clear_has_online() {
  _has_bits_[0] &= ~0x00000008u;
}
inline void WebappResponse::clear_online() {
  online_ = false;
  clear_has_online();
}
inline bool WebappResponse::online() const {
  return online_;
}
inline void WebappResponse::set_online(bool value) {
  set_has_online();
  online_ = value;
}

// optional bool authorized = 5;
inline bool WebappResponse::has_authorized() const {
  return (_has_bits_[0] & 0x00000010u) != 0;
}
inline void WebappResponse::set_has_authorized() {
  _has_bits_[0] |= 0x00000010u;
}
inline void WebappResponse::clear_has_authorized() {
  _has_bits_[0] &= ~0x00000010u;
}
inline void WebappResponse::clear_authorized() {
  authorized_ = false;
  clear_has_authorized();
}
inline bool WebappResponse::authorized() const {
  return authorized_;
}
inline void WebappResponse::set_authorized(bool value) {
  set_has_authorized();
  authorized_ = value;
}

// optional string channel_id = 6;
inline bool WebappResponse::has_channel_id() const {
  return (_has_bits_[0] & 0x00000020u) != 0;
}
inline void WebappResponse::set_has_channel_id() {
  _has_bits_[0] |= 0x00000020u;
}
inline void WebappResponse::clear_has_channel_id() {
  _has_bits_[0] &= ~0x00000020u;
}
inline void WebappResponse::clear_channel_id() {
  if (channel_id_ != &::google::protobuf::internal::kEmptyString) {
    channel_id_->clear();
  }
  clear_has_channel_id();
}
inline const ::std::string& WebappResponse::channel_id() const {
  return *channel_id_;
}
inline void WebappResponse::set_channel_id(const ::std::string& value) {
  set_has_channel_id();
  if (channel_id_ == &::google::protobuf::internal::kEmptyString) {
    channel_id_ = new ::std::string;
  }
  channel_id_->assign(value);
}
inline void WebappResponse::set_channel_id(const char* value) {
  set_has_channel_id();
  if (channel_id_ == &::google::protobuf::internal::kEmptyString) {
    channel_id_ = new ::std::string;
  }
  channel_id_->assign(value);
}
inline void WebappResponse::set_channel_id(const char* value, size_t size) {
  set_has_channel_id();
  if (channel_id_ == &::google::protobuf::internal::kEmptyString) {
    channel_id_ = new ::std::string;
  }
  channel_id_->assign(reinterpret_cast<const char*>(value), size);
}
inline ::std::string* WebappResponse::mutable_channel_id() {
  set_has_channel_id();
  if (channel_id_ == &::google::protobuf::internal::kEmptyString) {
    channel_id_ = new ::std::string;
  }
  return channel_id_;
}
inline ::std::string* WebappResponse::release_channel_id() {
  clear_has_channel_id();
  if (channel_id_ == &::google::protobuf::internal::kEmptyString) {
    return NULL;
  } else {
    ::std::string* temp = channel_id_;
    channel_id_ = const_cast< ::std::string*>(&::google::protobuf::internal::kEmptyString);
    return temp;
  }
}
inline void WebappResponse::set_allocated_channel_id(::std::string* channel_id) {
  if (channel_id_ != &::google::protobuf::internal::kEmptyString) {
    delete channel_id_;
  }
  if (channel_id) {
    set_has_channel_id();
    channel_id_ = channel_id;
  } else {
    clear_has_channel_id();
    channel_id_ = const_cast< ::std::string*>(&::google::protobuf::internal::kEmptyString);
  }
}

// repeated .chattp.ChattpMessage mesgs = 7;
inline int WebappResponse::mesgs_size() const {
  return mesgs_.size();
}
inline void WebappResponse::clear_mesgs() {
  mesgs_.Clear();
}
inline const ::chattp::ChattpMessage& WebappResponse::mesgs(int index) const {
  return mesgs_.Get(index);
}
inline ::chattp::ChattpMessage* WebappResponse::mutable_mesgs(int index) {
  return mesgs_.Mutable(index);
}
inline ::chattp::ChattpMessage* WebappResponse::add_mesgs() {
  return mesgs_.Add();
}
inline const ::google::protobuf::RepeatedPtrField< ::chattp::ChattpMessage >&
WebappResponse::mesgs() const {
  return mesgs_;
}
inline ::google::protobuf::RepeatedPtrField< ::chattp::ChattpMessage >*
WebappResponse::mutable_mesgs() {
  return &mesgs_;
}


// @@protoc_insertion_point(namespace_scope)

}  // namespace chattp

#ifndef SWIG
namespace google {
namespace protobuf {

template <>
inline const EnumDescriptor* GetEnumDescriptor< ::chattp::WebappRequest_WebappRequestType>() {
  return ::chattp::WebappRequest_WebappRequestType_descriptor();
}
template <>
inline const EnumDescriptor* GetEnumDescriptor< ::chattp::WebappResponse_WebappResponseType>() {
  return ::chattp::WebappResponse_WebappResponseType_descriptor();
}

}  // namespace google
}  // namespace protobuf
#endif  // SWIG

// @@protoc_insertion_point(global_scope)

#endif  // PROTOBUF_webapp_2eproto__INCLUDED
