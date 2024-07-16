package com.be.rebook.global.exception;

import lombok.AllArgsConstructor;
import lombok.Getter;
import org.springframework.http.HttpStatus;

@Getter
@AllArgsConstructor
public enum ErrorCode {

    /**
     * 멤버 에러
     */


    /**
     * 토큰 에러
     */

    /**
     * 상품 에러
     */
    NOT_EXIST_POST(HttpStatus.NOT_FOUND,  "존재하지 않는 상품입니다."),

    /**
     * 채팅 에러
     */

    /**
     * 일반 오류 코드
     */
    BAD_REQUEST(HttpStatus.BAD_REQUEST,  "잘못된 요청입니다"),
    NOT_FOUND(HttpStatus.NOT_FOUND, "찾을 수 없습니다"),
    METHOD_NOT_ALLOWED(HttpStatus.METHOD_NOT_ALLOWED,  "지원하지 않는 HTTP Method 요청입니다."),
    INTERNAL_SERVER_ERROR(HttpStatus.INTERNAL_SERVER_ERROR,  "내부 서버 오류입니다.");

    ;

    private final HttpStatus status;
    private final String message;


}
