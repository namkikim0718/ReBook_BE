package com.be.rebook.chat.service;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import lombok.extern.slf4j.Slf4j;
import org.springframework.http.*;
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
import org.springframework.web.client.RestTemplate;

@Service
@RequiredArgsConstructor
@Slf4j
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
            return chatRoomDto;
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
    public List<ChatMessageDTO> getChatRoomHistory(Long roomId, String username) {
        ChatRoom chatRoom = chatRoomRepository.findChatRoomById(roomId)
                .orElseThrow(() -> new BaseException(ErrorCode.CHAT_ROOM_NOT_FOUND));
        List<ChatMessage> messages = chatRoom.getMessages();

        // 각 메시지의 isRead 필드를 true로 업데이트
        // TODO: 메시지 수가 많아지게 되면 성능 이슈가 발생할 수 있음
        messages.forEach(message -> {
            if (!message.getSenderUsername().equals(username))
                message.setIsRead(true);
        });
        chatRoomRepository.save(chatRoom);

        List<ChatMessageDTO> messageDtos = messages.stream().map(ChatMessageDTO::new).toList();
        return messageDtos;
    }

    @Transactional
    public ChatRoomDto createChatRoom(CreateChatRoomDto createChatRoomDto) {
        String sellerUsername = createChatRoomDto.getSellerUsername();
        String buyerUsername = createChatRoomDto.getBuyerUsername();
        Long productId = createChatRoomDto.getProductId();

        ChatRoom chatRoom = chatRoomRepository.findChatRoomBySellerUsernameAndBuyerUsernameAndProductId(sellerUsername,
                buyerUsername, productId).orElse(null);

        if (chatRoom == null) {
            chatRoom = chatRoomRepository.save(new ChatRoom(createChatRoomDto));
        }
        return new ChatRoomDto(chatRoom);
    }

    @Transactional
    public ChatMessageDTO addMessageToChatRoom(ChatMessageDTO message) {
        ChatRoom chatRoom = chatRoomRepository.findById(message.getRoomId())
                .orElseThrow(() -> new BaseException(ErrorCode.CHAT_ROOM_NOT_FOUND));

        String senderUsername = message.getSenderUsername();
        if (!senderUsername.equals(chatRoom.getBuyerUsername())
                && !senderUsername.equals(chatRoom.getSellerUsername())) {
            throw new BaseException(ErrorCode.CHAT_SENDER_NOT_EIXIST);
        }

        // AI 모듈에 판별 요청
        RestTemplate restTemplate = new RestTemplate();
        String aiEndPoint = "https://192.168.1.55/predict";

        HashMap<Object, Object> requestData = new HashMap<>();
        requestData.put("chat_room_id", message.getRoomId());
        requestData.put("username", message.getSenderUsername());
        requestData.put("message", message.getMessage());

        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);

        HttpEntity<HashMap<Object, Object>> entity = new HttpEntity<>(requestData, headers);

        // POST 요청 보내기
        ResponseEntity<Map> response = restTemplate.exchange(
                aiEndPoint,
                HttpMethod.POST,
                entity,
                Map.class
        );

        Map<String, Object> aiResponse = response.getBody();
        Integer result = (Integer) aiResponse.get("result");
        String warningMessage = (String) aiResponse.get("warning_message");

        log.info("AI 모듈 접근 완료 {}", response);

        // 메시지 저장
        ChatMessage chatMessage = new ChatMessage(message, chatRoom);
        chatMessageRepository.save(chatMessage);

        // 메시지 전송 시 경고 메시지와 함께 반환
        ChatMessageDTO chatMessageDTO = new ChatMessageDTO(chatMessage);
        if (warningMessage != null) {
            chatMessageDTO.setWarningMessage(warningMessage);
            // 경고 메시지가 있으면 추가해서 저장
            chatMessage.updateWarningMessage(warningMessage);
        }



        return chatMessageDTO;
    }
}
