// Generated by the protocol buffer compiler.  DO NOT EDIT!
// source: message.proto

#define INTERNAL_SUPPRESS_PROTOBUF_FIELD_DEPRECATION
#include "message.pb.h"

#include <algorithm>

#include <google/protobuf/stubs/common.h>
#include <google/protobuf/stubs/once.h>
#include <google/protobuf/io/coded_stream.h>
#include <google/protobuf/wire_format_lite_inl.h>
#include <google/protobuf/descriptor.h>
#include <google/protobuf/generated_message_reflection.h>
#include <google/protobuf/reflection_ops.h>
#include <google/protobuf/wire_format.h>
// @@protoc_insertion_point(includes)

namespace chattp {

namespace {

const ::google::protobuf::Descriptor* ChattpMessage_descriptor_ = NULL;
const ::google::protobuf::internal::GeneratedMessageReflection*
  ChattpMessage_reflection_ = NULL;

}  // namespace


void protobuf_AssignDesc_message_2eproto() {
  protobuf_AddDesc_message_2eproto();
  const ::google::protobuf::FileDescriptor* file =
    ::google::protobuf::DescriptorPool::generated_pool()->FindFileByName(
      "message.proto");
  GOOGLE_CHECK(file != NULL);
  ChattpMessage_descriptor_ = file->message_type(0);
  static const int ChattpMessage_offsets_[5] = {
    GOOGLE_PROTOBUF_GENERATED_MESSAGE_FIELD_OFFSET(ChattpMessage, sender_),
    GOOGLE_PROTOBUF_GENERATED_MESSAGE_FIELD_OFFSET(ChattpMessage, receiver_),
    GOOGLE_PROTOBUF_GENERATED_MESSAGE_FIELD_OFFSET(ChattpMessage, group_message_),
    GOOGLE_PROTOBUF_GENERATED_MESSAGE_FIELD_OFFSET(ChattpMessage, body_),
    GOOGLE_PROTOBUF_GENERATED_MESSAGE_FIELD_OFFSET(ChattpMessage, timestamp_),
  };
  ChattpMessage_reflection_ =
    new ::google::protobuf::internal::GeneratedMessageReflection(
      ChattpMessage_descriptor_,
      ChattpMessage::default_instance_,
      ChattpMessage_offsets_,
      GOOGLE_PROTOBUF_GENERATED_MESSAGE_FIELD_OFFSET(ChattpMessage, _has_bits_[0]),
      GOOGLE_PROTOBUF_GENERATED_MESSAGE_FIELD_OFFSET(ChattpMessage, _unknown_fields_),
      -1,
      ::google::protobuf::DescriptorPool::generated_pool(),
      ::google::protobuf::MessageFactory::generated_factory(),
      sizeof(ChattpMessage));
}

namespace {

GOOGLE_PROTOBUF_DECLARE_ONCE(protobuf_AssignDescriptors_once_);
inline void protobuf_AssignDescriptorsOnce() {
  ::google::protobuf::GoogleOnceInit(&protobuf_AssignDescriptors_once_,
                 &protobuf_AssignDesc_message_2eproto);
}

void protobuf_RegisterTypes(const ::std::string&) {
  protobuf_AssignDescriptorsOnce();
  ::google::protobuf::MessageFactory::InternalRegisterGeneratedMessage(
    ChattpMessage_descriptor_, &ChattpMessage::default_instance());
}

}  // namespace

void protobuf_ShutdownFile_message_2eproto() {
  delete ChattpMessage::default_instance_;
  delete ChattpMessage_reflection_;
}

void protobuf_AddDesc_message_2eproto() {
  static bool already_here = false;
  if (already_here) return;
  already_here = true;
  GOOGLE_PROTOBUF_VERIFY_VERSION;

  ::google::protobuf::DescriptorPool::InternalAddGeneratedFile(
    "\n\rmessage.proto\022\006chattp\"p\n\rChattpMessage"
    "\022\016\n\006sender\030\001 \002(\t\022\020\n\010receiver\030\002 \002(\t\022\034\n\rgr"
    "oup_message\030\003 \001(\010:\005false\022\014\n\004body\030\004 \002(\t\022\021"
    "\n\ttimestamp\030\005 \002(\t", 137);
  ::google::protobuf::MessageFactory::InternalRegisterGeneratedFile(
    "message.proto", &protobuf_RegisterTypes);
  ChattpMessage::default_instance_ = new ChattpMessage();
  ChattpMessage::default_instance_->InitAsDefaultInstance();
  ::google::protobuf::internal::OnShutdown(&protobuf_ShutdownFile_message_2eproto);
}

// Force AddDescriptors() to be called at static initialization time.
struct StaticDescriptorInitializer_message_2eproto {
  StaticDescriptorInitializer_message_2eproto() {
    protobuf_AddDesc_message_2eproto();
  }
} static_descriptor_initializer_message_2eproto_;

// ===================================================================

#ifndef _MSC_VER
const int ChattpMessage::kSenderFieldNumber;
const int ChattpMessage::kReceiverFieldNumber;
const int ChattpMessage::kGroupMessageFieldNumber;
const int ChattpMessage::kBodyFieldNumber;
const int ChattpMessage::kTimestampFieldNumber;
#endif  // !_MSC_VER

ChattpMessage::ChattpMessage()
  : ::google::protobuf::Message() {
  SharedCtor();
}

void ChattpMessage::InitAsDefaultInstance() {
}

ChattpMessage::ChattpMessage(const ChattpMessage& from)
  : ::google::protobuf::Message() {
  SharedCtor();
  MergeFrom(from);
}

void ChattpMessage::SharedCtor() {
  _cached_size_ = 0;
  sender_ = const_cast< ::std::string*>(&::google::protobuf::internal::kEmptyString);
  receiver_ = const_cast< ::std::string*>(&::google::protobuf::internal::kEmptyString);
  group_message_ = false;
  body_ = const_cast< ::std::string*>(&::google::protobuf::internal::kEmptyString);
  timestamp_ = const_cast< ::std::string*>(&::google::protobuf::internal::kEmptyString);
  ::memset(_has_bits_, 0, sizeof(_has_bits_));
}

ChattpMessage::~ChattpMessage() {
  SharedDtor();
}

void ChattpMessage::SharedDtor() {
  if (sender_ != &::google::protobuf::internal::kEmptyString) {
    delete sender_;
  }
  if (receiver_ != &::google::protobuf::internal::kEmptyString) {
    delete receiver_;
  }
  if (body_ != &::google::protobuf::internal::kEmptyString) {
    delete body_;
  }
  if (timestamp_ != &::google::protobuf::internal::kEmptyString) {
    delete timestamp_;
  }
  if (this != default_instance_) {
  }
}

void ChattpMessage::SetCachedSize(int size) const {
  GOOGLE_SAFE_CONCURRENT_WRITES_BEGIN();
  _cached_size_ = size;
  GOOGLE_SAFE_CONCURRENT_WRITES_END();
}
const ::google::protobuf::Descriptor* ChattpMessage::descriptor() {
  protobuf_AssignDescriptorsOnce();
  return ChattpMessage_descriptor_;
}

const ChattpMessage& ChattpMessage::default_instance() {
  if (default_instance_ == NULL) protobuf_AddDesc_message_2eproto();
  return *default_instance_;
}

ChattpMessage* ChattpMessage::default_instance_ = NULL;

ChattpMessage* ChattpMessage::New() const {
  return new ChattpMessage;
}

void ChattpMessage::Clear() {
  if (_has_bits_[0 / 32] & (0xffu << (0 % 32))) {
    if (has_sender()) {
      if (sender_ != &::google::protobuf::internal::kEmptyString) {
        sender_->clear();
      }
    }
    if (has_receiver()) {
      if (receiver_ != &::google::protobuf::internal::kEmptyString) {
        receiver_->clear();
      }
    }
    group_message_ = false;
    if (has_body()) {
      if (body_ != &::google::protobuf::internal::kEmptyString) {
        body_->clear();
      }
    }
    if (has_timestamp()) {
      if (timestamp_ != &::google::protobuf::internal::kEmptyString) {
        timestamp_->clear();
      }
    }
  }
  ::memset(_has_bits_, 0, sizeof(_has_bits_));
  mutable_unknown_fields()->Clear();
}

bool ChattpMessage::MergePartialFromCodedStream(
    ::google::protobuf::io::CodedInputStream* input) {
#define DO_(EXPRESSION) if (!(EXPRESSION)) return false
  ::google::protobuf::uint32 tag;
  while ((tag = input->ReadTag()) != 0) {
    switch (::google::protobuf::internal::WireFormatLite::GetTagFieldNumber(tag)) {
      // required string sender = 1;
      case 1: {
        if (::google::protobuf::internal::WireFormatLite::GetTagWireType(tag) ==
            ::google::protobuf::internal::WireFormatLite::WIRETYPE_LENGTH_DELIMITED) {
          DO_(::google::protobuf::internal::WireFormatLite::ReadString(
                input, this->mutable_sender()));
          ::google::protobuf::internal::WireFormat::VerifyUTF8String(
            this->sender().data(), this->sender().length(),
            ::google::protobuf::internal::WireFormat::PARSE);
        } else {
          goto handle_uninterpreted;
        }
        if (input->ExpectTag(18)) goto parse_receiver;
        break;
      }

      // required string receiver = 2;
      case 2: {
        if (::google::protobuf::internal::WireFormatLite::GetTagWireType(tag) ==
            ::google::protobuf::internal::WireFormatLite::WIRETYPE_LENGTH_DELIMITED) {
         parse_receiver:
          DO_(::google::protobuf::internal::WireFormatLite::ReadString(
                input, this->mutable_receiver()));
          ::google::protobuf::internal::WireFormat::VerifyUTF8String(
            this->receiver().data(), this->receiver().length(),
            ::google::protobuf::internal::WireFormat::PARSE);
        } else {
          goto handle_uninterpreted;
        }
        if (input->ExpectTag(24)) goto parse_group_message;
        break;
      }

      // optional bool group_message = 3 [default = false];
      case 3: {
        if (::google::protobuf::internal::WireFormatLite::GetTagWireType(tag) ==
            ::google::protobuf::internal::WireFormatLite::WIRETYPE_VARINT) {
         parse_group_message:
          DO_((::google::protobuf::internal::WireFormatLite::ReadPrimitive<
                   bool, ::google::protobuf::internal::WireFormatLite::TYPE_BOOL>(
                 input, &group_message_)));
          set_has_group_message();
        } else {
          goto handle_uninterpreted;
        }
        if (input->ExpectTag(34)) goto parse_body;
        break;
      }

      // required string body = 4;
      case 4: {
        if (::google::protobuf::internal::WireFormatLite::GetTagWireType(tag) ==
            ::google::protobuf::internal::WireFormatLite::WIRETYPE_LENGTH_DELIMITED) {
         parse_body:
          DO_(::google::protobuf::internal::WireFormatLite::ReadString(
                input, this->mutable_body()));
          ::google::protobuf::internal::WireFormat::VerifyUTF8String(
            this->body().data(), this->body().length(),
            ::google::protobuf::internal::WireFormat::PARSE);
        } else {
          goto handle_uninterpreted;
        }
        if (input->ExpectTag(42)) goto parse_timestamp;
        break;
      }

      // required string timestamp = 5;
      case 5: {
        if (::google::protobuf::internal::WireFormatLite::GetTagWireType(tag) ==
            ::google::protobuf::internal::WireFormatLite::WIRETYPE_LENGTH_DELIMITED) {
         parse_timestamp:
          DO_(::google::protobuf::internal::WireFormatLite::ReadString(
                input, this->mutable_timestamp()));
          ::google::protobuf::internal::WireFormat::VerifyUTF8String(
            this->timestamp().data(), this->timestamp().length(),
            ::google::protobuf::internal::WireFormat::PARSE);
        } else {
          goto handle_uninterpreted;
        }
        if (input->ExpectAtEnd()) return true;
        break;
      }

      default: {
      handle_uninterpreted:
        if (::google::protobuf::internal::WireFormatLite::GetTagWireType(tag) ==
            ::google::protobuf::internal::WireFormatLite::WIRETYPE_END_GROUP) {
          return true;
        }
        DO_(::google::protobuf::internal::WireFormat::SkipField(
              input, tag, mutable_unknown_fields()));
        break;
      }
    }
  }
  return true;
#undef DO_
}

void ChattpMessage::SerializeWithCachedSizes(
    ::google::protobuf::io::CodedOutputStream* output) const {
  // required string sender = 1;
  if (has_sender()) {
    ::google::protobuf::internal::WireFormat::VerifyUTF8String(
      this->sender().data(), this->sender().length(),
      ::google::protobuf::internal::WireFormat::SERIALIZE);
    ::google::protobuf::internal::WireFormatLite::WriteString(
      1, this->sender(), output);
  }

  // required string receiver = 2;
  if (has_receiver()) {
    ::google::protobuf::internal::WireFormat::VerifyUTF8String(
      this->receiver().data(), this->receiver().length(),
      ::google::protobuf::internal::WireFormat::SERIALIZE);
    ::google::protobuf::internal::WireFormatLite::WriteString(
      2, this->receiver(), output);
  }

  // optional bool group_message = 3 [default = false];
  if (has_group_message()) {
    ::google::protobuf::internal::WireFormatLite::WriteBool(3, this->group_message(), output);
  }

  // required string body = 4;
  if (has_body()) {
    ::google::protobuf::internal::WireFormat::VerifyUTF8String(
      this->body().data(), this->body().length(),
      ::google::protobuf::internal::WireFormat::SERIALIZE);
    ::google::protobuf::internal::WireFormatLite::WriteString(
      4, this->body(), output);
  }

  // required string timestamp = 5;
  if (has_timestamp()) {
    ::google::protobuf::internal::WireFormat::VerifyUTF8String(
      this->timestamp().data(), this->timestamp().length(),
      ::google::protobuf::internal::WireFormat::SERIALIZE);
    ::google::protobuf::internal::WireFormatLite::WriteString(
      5, this->timestamp(), output);
  }

  if (!unknown_fields().empty()) {
    ::google::protobuf::internal::WireFormat::SerializeUnknownFields(
        unknown_fields(), output);
  }
}

::google::protobuf::uint8* ChattpMessage::SerializeWithCachedSizesToArray(
    ::google::protobuf::uint8* target) const {
  // required string sender = 1;
  if (has_sender()) {
    ::google::protobuf::internal::WireFormat::VerifyUTF8String(
      this->sender().data(), this->sender().length(),
      ::google::protobuf::internal::WireFormat::SERIALIZE);
    target =
      ::google::protobuf::internal::WireFormatLite::WriteStringToArray(
        1, this->sender(), target);
  }

  // required string receiver = 2;
  if (has_receiver()) {
    ::google::protobuf::internal::WireFormat::VerifyUTF8String(
      this->receiver().data(), this->receiver().length(),
      ::google::protobuf::internal::WireFormat::SERIALIZE);
    target =
      ::google::protobuf::internal::WireFormatLite::WriteStringToArray(
        2, this->receiver(), target);
  }

  // optional bool group_message = 3 [default = false];
  if (has_group_message()) {
    target = ::google::protobuf::internal::WireFormatLite::WriteBoolToArray(3, this->group_message(), target);
  }

  // required string body = 4;
  if (has_body()) {
    ::google::protobuf::internal::WireFormat::VerifyUTF8String(
      this->body().data(), this->body().length(),
      ::google::protobuf::internal::WireFormat::SERIALIZE);
    target =
      ::google::protobuf::internal::WireFormatLite::WriteStringToArray(
        4, this->body(), target);
  }

  // required string timestamp = 5;
  if (has_timestamp()) {
    ::google::protobuf::internal::WireFormat::VerifyUTF8String(
      this->timestamp().data(), this->timestamp().length(),
      ::google::protobuf::internal::WireFormat::SERIALIZE);
    target =
      ::google::protobuf::internal::WireFormatLite::WriteStringToArray(
        5, this->timestamp(), target);
  }

  if (!unknown_fields().empty()) {
    target = ::google::protobuf::internal::WireFormat::SerializeUnknownFieldsToArray(
        unknown_fields(), target);
  }
  return target;
}

int ChattpMessage::ByteSize() const {
  int total_size = 0;

  if (_has_bits_[0 / 32] & (0xffu << (0 % 32))) {
    // required string sender = 1;
    if (has_sender()) {
      total_size += 1 +
        ::google::protobuf::internal::WireFormatLite::StringSize(
          this->sender());
    }

    // required string receiver = 2;
    if (has_receiver()) {
      total_size += 1 +
        ::google::protobuf::internal::WireFormatLite::StringSize(
          this->receiver());
    }

    // optional bool group_message = 3 [default = false];
    if (has_group_message()) {
      total_size += 1 + 1;
    }

    // required string body = 4;
    if (has_body()) {
      total_size += 1 +
        ::google::protobuf::internal::WireFormatLite::StringSize(
          this->body());
    }

    // required string timestamp = 5;
    if (has_timestamp()) {
      total_size += 1 +
        ::google::protobuf::internal::WireFormatLite::StringSize(
          this->timestamp());
    }

  }
  if (!unknown_fields().empty()) {
    total_size +=
      ::google::protobuf::internal::WireFormat::ComputeUnknownFieldsSize(
        unknown_fields());
  }
  GOOGLE_SAFE_CONCURRENT_WRITES_BEGIN();
  _cached_size_ = total_size;
  GOOGLE_SAFE_CONCURRENT_WRITES_END();
  return total_size;
}

void ChattpMessage::MergeFrom(const ::google::protobuf::Message& from) {
  GOOGLE_CHECK_NE(&from, this);
  const ChattpMessage* source =
    ::google::protobuf::internal::dynamic_cast_if_available<const ChattpMessage*>(
      &from);
  if (source == NULL) {
    ::google::protobuf::internal::ReflectionOps::Merge(from, this);
  } else {
    MergeFrom(*source);
  }
}

void ChattpMessage::MergeFrom(const ChattpMessage& from) {
  GOOGLE_CHECK_NE(&from, this);
  if (from._has_bits_[0 / 32] & (0xffu << (0 % 32))) {
    if (from.has_sender()) {
      set_sender(from.sender());
    }
    if (from.has_receiver()) {
      set_receiver(from.receiver());
    }
    if (from.has_group_message()) {
      set_group_message(from.group_message());
    }
    if (from.has_body()) {
      set_body(from.body());
    }
    if (from.has_timestamp()) {
      set_timestamp(from.timestamp());
    }
  }
  mutable_unknown_fields()->MergeFrom(from.unknown_fields());
}

void ChattpMessage::CopyFrom(const ::google::protobuf::Message& from) {
  if (&from == this) return;
  Clear();
  MergeFrom(from);
}

void ChattpMessage::CopyFrom(const ChattpMessage& from) {
  if (&from == this) return;
  Clear();
  MergeFrom(from);
}

bool ChattpMessage::IsInitialized() const {
  if ((_has_bits_[0] & 0x0000001b) != 0x0000001b) return false;

  return true;
}

void ChattpMessage::Swap(ChattpMessage* other) {
  if (other != this) {
    std::swap(sender_, other->sender_);
    std::swap(receiver_, other->receiver_);
    std::swap(group_message_, other->group_message_);
    std::swap(body_, other->body_);
    std::swap(timestamp_, other->timestamp_);
    std::swap(_has_bits_[0], other->_has_bits_[0]);
    _unknown_fields_.Swap(&other->_unknown_fields_);
    std::swap(_cached_size_, other->_cached_size_);
  }
}

::google::protobuf::Metadata ChattpMessage::GetMetadata() const {
  protobuf_AssignDescriptorsOnce();
  ::google::protobuf::Metadata metadata;
  metadata.descriptor = ChattpMessage_descriptor_;
  metadata.reflection = ChattpMessage_reflection_;
  return metadata;
}


// @@protoc_insertion_point(namespace_scope)

}  // namespace chattp

// @@protoc_insertion_point(global_scope)
