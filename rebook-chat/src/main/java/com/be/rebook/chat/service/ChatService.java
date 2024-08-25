package com.be.rebook.chat.service;

import java.util.List;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.be.rebook.chat.dto.ChatMessageDTO;
import com.be.rebook.chat.dto.ChatRoomDto;
import com.be.rebook.chat.dto.CreateChatRoomDto;
import com.be.rebook.chat.entity.ChatMessage;
import com.be.rebook.chat.entity.ChatRoom;
import com.be.rebook.chat.repository.ChatMessageRepository;
import com.be.rebook.chat.repository.ChatRoomRepository;
import com.be.rebook.common.exception.BaseException;
import com.be.rebook.common.exception.ErrorCode;

import lombok.RequiredArgsConstructor;

@Service
@RequiredArgsConstructor
public class ChatService {

    private final ChatRoomRepository chatRoomRepository;

    private final ChatMessageRepository chatMessageRepository;

    @Transactional(readOnly = true)
    public ChatRoomDto findChatRoomById(Long id) {
        ChatRoom chatRoom = chatRoomRepository.findById(id)
                .orElseThrow(() -> new BaseException(ErrorCode.CHAT_ROOM_NOT_FOUND)); // TODO : 적절한 예외처리 변경
        return new ChatRoomDto(chatRoom);
    }

    @Transactional(readOnly = true)
    public List<ChatRoomDto> findChatRoomsByUsername(String username) {
        List<ChatRoom> chatRooms = chatRoomRepository.findChatRoomsByMemberUsername(username);

        List<ChatRoomDto> chatRoomDtos = chatRooms.stream().map((chatRoom) -> {
            ChatRoomDto chatRoomDto = new ChatRoomDto(chatRoom);
            Integer unreadCount = chatMessageRepository
                    .countUnreadMessagesByChatRoomIdAndNotSenderUsername(chatRoom.getId(), username);
            chatRoomDto.setUnreadCount(unreadCount);
            return new ChatRoomDto(chatRoom);
        }).toList();

        return chatRoomDtos;
    }

    @Transactional
    public void patchMessageRead(Long chatMessageId) {
        ChatMessage chatMessage = chatMessageRepository.findById(chatMessageId)
                .orElseThrow(() -> new BaseException(ErrorCode.CHAT_MESSAGE_NOT_FOUND));
        chatMessage.setIsRead(true);
        chatMessageRepository.save(chatMessage);
    }

    @Transactional
    public List<ChatMessageDTO> getChatRoomHistory(Long roomId) {
        ChatRoom chatRoom = chatRoomRepository.findChatRoomById(roomId)
                .orElseThrow(() -> new BaseException(ErrorCode.CHAT_ROOM_NOT_FOUND));
        List<ChatMessage> messages = chatRoom.getMessages();

        // 각 메시지의 isRead 필드를 true로 업데이트
        // TODO: 메시지 수가 많아지게 되면 성능 이슈가 발생할 수 있음
        messages.forEach(message -> message.setIsRead(true));
        chatRoomRepository.save(chatRoom);

        List<ChatMessageDTO> messageDtos = messages.stream().map(ChatMessageDTO::new).toList();
        return messageDtos;
    }

    @Transactional
    public Long createChatRoom(CreateChatRoomDto createChatRoomDto) {
        ChatRoom newChatRoom = new ChatRoom(createChatRoomDto);
        newChatRoom = chatRoomRepository.save(newChatRoom);
        return newChatRoom.getId();
    }

    @Transactional
    public Long addMessageToChatRoom(ChatMessageDTO message) {
        ChatRoom chatRoom = chatRoomRepository.findById(message.getRoomId())
                .orElseThrow(() -> new BaseException(ErrorCode.CHAT_ROOM_NOT_FOUND));

        String senderUsername = message.getSenderUsername();
        if (!senderUsername.equals(chatRoom.getBuyerUsername())
                && !senderUsername.equals(chatRoom.getSellerUsername())) {
            throw new BaseException(ErrorCode.CHAT_SENDER_NOT_EIXIST);
        }

        chatRoom.addMessage(message);
        return chatRoomRepository.save(chatRoom).getId();
    }
}
